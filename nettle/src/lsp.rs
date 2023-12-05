#![allow(unused, dead_code)]

use serde_json::{Value, json};
use nettle::Circuit;
use nettle::{Loc, HasLoc, SourceInfo};

use std::sync::mpsc::channel;
use std::thread;
use std::collections::HashMap;
use std::io::BufRead;
use std::io::{stdin, stdout};
use log::*;

fn main() {
    init_logging();
    std::panic::set_hook(Box::new(panic_handler));
    info!("Starting nettle-lsp");

    let (message_send, message_recv) = channel::<Value>();

    let thread = thread::spawn(move || {
        loop {
            let message = read_message();
            message_send.send(message);
        }
    });

    let mut state = State::new();

    loop {
        let message = message_recv.recv().unwrap();
        info!("Handling message: {message:?}");
        match message.get("method") {
            None => break,
            Some(method) => {
                let method = method.as_str().unwrap();
                info!("Method: {method:?}");
                match method {
                    "initialize" => state.initialize(message),
                    "initialized" => (),
                    "shutdown" => break,
                    "textDocument/didOpen" => state.text_document_did_open(message),
                    "textDocument/didChange" => state.text_document_did_change(message),
                    "textDocument/didSave" => state.text_document_did_save(message),
                    "textDocument/hover" => state.text_document_hover(message),
                    "textDocument/definition" => state.text_document_definition(message),
                    _ => (),
                }
            },
        }
    }
}

fn read_message() -> Value {
    use std::io::Read;
    let mut stdin = stdin().lock();

    let mut buffer = String::new();
//    let mut headers: HashMap<String, String> = HashMap::new();
    stdin.read_line(&mut buffer);
    assert!(buffer.starts_with("Content-Length: "));
    let length = buffer.split(": ").collect::<Vec<_>>()[1].trim().parse::<usize>().unwrap();
    // throw away empty line
    stdin.read_line(&mut buffer);
    let mut buffer: Vec<u8> = vec![0; length];
    stdin.read(&mut buffer);
    let buffer = String::from_utf8(buffer).unwrap();
    let message: Value = serde_json::from_str(&buffer).unwrap();
    let method = &message["method"];
    debug!("{}", serde_json::to_string_pretty(&message).unwrap());
    info!("Received message with method: {}", method.to_string());
    message
}

fn send_message(message: Value) {
    use std::io::Write;

    let method = &message["method"];
    let value_str = serde_json::to_string_pretty(&message).unwrap();
    let value_str_len = value_str.len();

    print!("Content-Length: {value_str_len}\r\n\r\n{value_str}");
    stdout().flush().unwrap();

    debug!("{}", serde_json::to_string_pretty(&message).unwrap());
    info!("Sending message with method: {}", method.to_string());
}

fn init_logging() {
    use chrono::{DateTime, Utc};

    let mut dispatch = fern::Dispatch::new()
        .format(|out, message, record| {
            let now: DateTime<Utc> = Utc::now();
            out.finish(format_args!(
                "[{} {} {}] {}",
                now.format("%Y-%m-%dT%H:%M:%S%.fZ"),
                record.level(),
                record.target(),
                message
            ))
        })
        .chain(fern::log_file("/home/tac-tics/nettle-lsp.txt").unwrap());

    let level = std::env::var("LEVEL").unwrap_or_default().to_string();

    if level == "DEBUG" {
        dispatch = dispatch.level(log::LevelFilter::Debug);
    } else {
        dispatch = dispatch.level(log::LevelFilter::Info);
    }

    dispatch.apply().unwrap();
}

type Uri = String;

struct State {
    buffers: HashMap<Uri, Buffer>,
}

impl State {
    fn new() -> State {
        State {
            buffers: HashMap::new(),
        }
    }

    fn buffer(&mut self, uri: &Uri) -> &mut Buffer {
        self.buffers.get_mut(uri).unwrap_or_else(|| {
            error!("No such URI: {uri}");
            panic!("No such URI: {uri}")
        })
    }

    fn open_buffer(&mut self, uri: &Uri, text: &str) -> &mut Buffer {
        let buffer = Buffer::new(uri, text);
        self.buffers.insert(uri.clone(), buffer);
        self.buffers.get_mut(uri).unwrap_or_else(|| {
            error!("No such URI: {uri}");
            panic!("No such URI: {uri}")
        })
    }
}

struct Buffer {
    uri: String,
    circuit: Circuit,
    text: String,
}

impl Buffer {
    fn new(uri: &Uri, text: &str) -> Buffer {
        let circuit = match nettle::parse_top(text) {
            Ok(circuit) => circuit,
            Err(_e) => nettle::parse_top("mod Top {}").unwrap(),
        };

        Buffer {
            uri: uri.to_string(),
            circuit,
            text: text.to_string(),
        }
    }

    fn update_text(&mut self, text: String) {
        debug!("{}", self.text);
        self.text = text.to_string();
    }

    fn send_diagnostics(&mut self) {
        let mut diagnostics = vec![];

        self.circuit = match nettle::parse_top(&self.text) {
            Ok(circuit) => circuit,
            Err(e) => {
                info!("Parse Error: {e:?}");
                let source_info = SourceInfo::from_string(&self.text);
                let (start_line, start_col, end_line, end_col, message) = match &e {
                    lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                        let start_idx = token.0;
                        let end_idx = token.2;
                        let start_linecol = source_info.linecol_from(start_idx);
                        let end_linecol = source_info.linecol_from(end_idx);

                        let message = format!("Parse error: Expected one of {}", expected.join(" "));
                        (start_linecol.line() - 1, start_linecol.col() - 1,
                         end_linecol.line() - 1, end_linecol.col() - 1,
                         message)
                    },
                    _ => (0, 0, 0, 0, format!("{e:?}")),
                };

                let diagnostic = json!({
                    "range": {
                        "start": { "line": start_line, "character": start_col },
                        "end": { "line": end_line, "character": end_col },
                    },
                    "severity": 1, // ERROR
                    "message": message,
                });
                diagnostics.push(diagnostic);
                let message = json!({
                    "jsonrpc": "2.0",
                    "method": "textDocument/publishDiagnostics",
                    "params": {
                        "uri": self.uri.to_string(),
                        "diagnostics": diagnostics,
                    },
                });
                send_message(message);
                return;
            },
        };

        if let Err(errors) = self.circuit.check() {
            warn!("Errors found: {}", errors.len());
            for (path, error) in &errors {
                let loc: Loc = error.loc();
                warn!("    {:?}: {path}: {error:?}", error.loc());

                let start_line = loc.start().line() - 1;
                let start_character = loc.start().col() - 1;

                let end_line = loc.end().line() - 1;
                let end_character = loc.end().col() - 1;

                let message = format!("{error:?}");

                let diagnostic = json!({
                    "range": {
                        "start": { "line": start_line, "character": start_character },
                        "end": { "line": end_line, "character": end_character },
                    },
                    "severity": 1, // ERROR
                    "message": message,
                });
                diagnostics.push(diagnostic);
            }
        }

        let message = json!({
            "jsonrpc": "2.0",
            "method": "textDocument/publishDiagnostics",
            "params": {
                "uri": self.uri.to_string(),
                "diagnostics": diagnostics,
            },
        });
        send_message(message);
    }
}

fn pos_to_lineno_col(text: &str, pos: usize) -> (usize, usize) {
    let mut lineno = 0;
    let mut col = 0;
    let mut cur_pos = 0;

    let lines = text.split("\n");
    for line in lines {
        if cur_pos + line.len() < pos {
            lineno += 1;
            cur_pos += line.len() + 1; // + 1 for the newline
        } else {
            break;
        }
    }

    (lineno, pos - cur_pos)
}

impl State {
    fn initialize(&mut self, request: Value) {
        let response: Value = json!({
            "jsonrpc": "2.0",
            "id": request["id"],
            "result": {
                "capabilities": {
                    "positionEncoding": "utf-8",
                    "textDocumentSync": 1, // TextDocumentSyncKind.FULL
                    "hoverProvider": true,
                    "declarationProvider": true,
                    "definitionProvider": true,
                    "typeDefinitionProvider": true,
                    "referencesProvider": true,
                    "documentHighlightProvider": true,
                    "documentSymbolProvider": true,
//                    "renameProvider": true,
                },
            },
        });

        send_message(response);
    }

    fn text_document_did_open(&mut self, message: Value) {
        debug!("{}", serde_json::to_string_pretty(&message).unwrap());
        let text = message["params"]["textDocument"]["text"].as_str().unwrap();
        let uri = message["params"]["textDocument"]["uri"].as_str().unwrap();
        let buffer = self.open_buffer(&uri.to_string(), text);
        warn!("text is {text:?}");
        buffer.send_diagnostics();
    }

    fn text_document_did_change(&mut self, message: Value) {
        debug!("{}", serde_json::to_string_pretty(&message).unwrap());
        let uri = message["params"]["textDocument"]["uri"].as_str().unwrap().to_string();
        let mut buffer = self.buffer(&uri);
        buffer.update_text(message["params"]["contentChanges"][0]["text"].as_str().unwrap().to_string());
        buffer.send_diagnostics();
    }

    fn text_document_did_save(&mut self, message: Value) {
        debug!("{}", serde_json::to_string_pretty(&message).unwrap());
        let uri = message["params"]["textDocument"]["uri"].as_str().unwrap().to_string();
        let mut buffer = self.buffer(&uri);
        buffer.send_diagnostics();
    }

    fn text_document_hover(&mut self, message: Value) {
        let response: Value = json!({
            "jsonrpc": "2.0",
            "id": message["id"],
            "result": {
                "contents": {
                    "kind": "markdown",
                    "value": "Please construct additional pylons",
                },
            },
        });

        send_message(response);
    }

    fn text_document_definition(&mut self, message: Value) {
        let (start_line, start_character) = (0, 0);
        let (end_line, end_character) = (0, 1);
        let uri = message["params"]["textDocument"]["uri"].as_str().unwrap().to_string();
        let response: Value = json!({
            "jsonrpc": "2.0",
            "id": message["id"],
            "result": {
                "uri": uri,
                "range": {
                    "start": { "line": start_line, "character": start_character },
                    "end": { "line": end_line, "character": end_character },
                },
            },
        });

        send_message(response);
    }
}

fn panic_handler(info: &std::panic::PanicInfo) {
    error!("Panic occurred: {}", info);
}
