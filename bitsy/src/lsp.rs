// TODO Remove this.

use serde_json::{Value, json};
use bitsy_lang::Package;
use bitsy_lang::HasSpan;
use bitsy_lang::LineCol;

use std::sync::mpsc::channel;
use std::thread;
use std::collections::HashMap;
use std::io::BufRead;
use std::io::{stdin, stdout};
use log::*;

pub(crate) fn run_lsp() {
    init_logging();
    std::panic::set_hook(Box::new(panic_handler));
    info!("Starting bitsy-lsp");

    let (message_send, message_recv) = channel::<Value>();

    let _thread = thread::spawn(move || {
        loop {
            let message = read_message();
            message_send.send(message).unwrap();
        }
    });

    let mut state = State::new();

    loop {
        match message_recv.recv() {
            Ok(message) => {
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
            },
            Err(_e) => return,
        }
    }
}

fn read_message() -> Value {
    use std::io::Read;
    let mut stdin = stdin().lock();

    let mut buffer = String::new();
//    let mut headers: HashMap<String, String> = HashMap::new();
    stdin.read_line(&mut buffer).unwrap();
    assert!(buffer.starts_with("Content-Length: "));
    let length = buffer.split(": ").collect::<Vec<_>>()[1].trim().parse::<usize>().unwrap();
    // throw away empty line
    stdin.read_line(&mut buffer).unwrap();
    let mut buffer: Vec<u8> = vec![0; length];
    let mut bytes_read = 0;
    while bytes_read < length {
        bytes_read += stdin.read(&mut buffer[bytes_read..]).unwrap();
    }
    assert_eq!(bytes_read, length);
    assert_eq!(buffer.len(), length);
    let buffer = String::from_utf8(buffer).unwrap();
    let message: Value = serde_json::from_str(&buffer).unwrap();
    let method = &message["method"];
    //debug!("{}", serde_json::to_string_pretty(&message).unwrap());
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
        .chain(fern::log_file("/home/tac-tics/bitsy-lsp.txt").unwrap());

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
    package: Package,
    text: String,
}

impl Buffer {
    fn new(uri: &Uri, text: &str) -> Buffer {
        // TODO should fake parsing package from file
        let package = match bitsy_lang::load_package_from_string(text) {
            Ok(package) => package,
            Err(_e) => bitsy_lang::load_package_from_string("mod Top {}").unwrap(),
        };

        Buffer {
            uri: uri.to_string(),
            package,
            text: text.to_string(),
        }
    }

    fn update_text(&mut self, text: String) {
        debug!("{}", self.text);
        self.text = text.to_string();
    }

    fn send_diagnostics(&mut self) {
        let mut diagnostics = vec![];
        if let Err(errors) = bitsy_lang::ast::parse_package_from_string(&self.text) {
                info!("Errors: {errors:?}");
                for error in errors {
                    let start_line = error.span().start().line() - 1;
                    let start_character = error.span().start().col() - 1;

                    let end_line = error.span().end().line() - 1;
                    let end_character = error.span().end().col() - 1;

                    let message = format!("{error}");

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
        }

        // TODO should fake parsing package from file
        self.package = match bitsy_lang::load_package_from_string(&self.text) {
            Ok(package) => package,
            Err(errors) => {
                info!("Errors: {errors:?}");
                for error in errors {
                    let start_line = error.span().start().line() - 1;
                    let start_character = error.span().start().col() - 1;

                    let end_line = error.span().end().line() - 1;
                    let end_character = error.span().end().col() - 1;

                    let message = format!("{error}");

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
        let buffer = self.buffer(&uri);
        buffer.update_text(message["params"]["contentChanges"][0]["text"].as_str().unwrap().to_string());
        buffer.send_diagnostics();
    }

    fn text_document_did_save(&mut self, message: Value) {
        debug!("{}", serde_json::to_string_pretty(&message).unwrap());
        let uri = message["params"]["textDocument"]["uri"].as_str().unwrap().to_string();
        let buffer = self.buffer(&uri);
        buffer.send_diagnostics();
    }

    fn text_document_hover(&mut self, message: Value) {
        let line = message["params"]["position"]["line"].as_u64().unwrap() + 1;
        let character = message["params"]["position"]["character"].as_u64().unwrap() + 1;
        let linecol = LineCol::from(line.try_into().unwrap(), character.try_into().unwrap());

        let uri = message["params"]["textDocument"]["uri"].as_str().unwrap().to_string();
        let buffer = self.buffer(&uri);

        let mut hover_ident = None;

        if let Ok(package) = bitsy_lang::load_package_from_string(&buffer.text) {
            for ident in package.idents().iter() {
                if ident.span().contains(&linecol) {
                    hover_ident = Some(ident.clone());
                }
            }
        }

        warn!("Hover: {linecol}: {:?}", hover_ident);
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
        let line = message["params"]["position"]["line"].as_u64().unwrap() + 1;
        let character = message["params"]["position"]["character"].as_u64().unwrap() + 1;
        let linecol = LineCol::from(line.try_into().unwrap(), character.try_into().unwrap());

        let uri = message["params"]["textDocument"]["uri"].as_str().unwrap().to_string();
        let buffer = self.buffer(&uri);

        let mut item_span = None;
        if let Ok(package) = bitsy_lang::load_package_from_string(&buffer.text) {
            for ident in package.idents().iter() {
                if ident.span().contains(&linecol) {
                    if let Some(item) = package.item(&ident.name) {
                        item_span = Some(item.span());
                    }
                }
            }
        }

        let result = if let Some(span) = item_span {
            let start_line = span.start().line() - 1;
            let start_character = span.start().col() - 1;
            let end_line = span.end().line() - 1;
            let end_character = span.end().col() - 1;

            json!({
                "uri": uri,
                "range": {
                    "start": { "line": start_line, "character": start_character },
                    "end": { "line": end_line, "character": end_character },
                },
            })
        } else {
            json!(null)
        };

        let response: Value = json!({
            "jsonrpc": "2.0",
            "id": message["id"],
            "result": result,
        });

        send_message(response);
    }
}

fn panic_handler(info: &std::panic::PanicInfo) {
    error!("Panic occurred: {}", info);
}
