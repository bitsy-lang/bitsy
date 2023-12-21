use termion::event::{Key, Event};
use termion::clear;
use termion::input::TermRead;
use termion::cursor::DetectCursorPos;
use termion::raw::IntoRawMode;
use std::io::{Write, stdout, stdin};

struct Editor {
    prompt: String,
    lines: Vec<String>,
    i: usize,
    col: usize,
}

impl Editor {
    pub fn new() -> Editor {
        let lines = vec![String::new()];
        let i = 0;
        let col = 0;
        let prompt = "> ".to_string();
        Editor {
            prompt,
            lines,
            i,
            col,
        }
    }

    pub fn prompt(&mut self, prompt: &str) {
        self.prompt = prompt.to_string();
    }

    pub fn line(&mut self) -> &str{
        &self.lines[self.i]
    }

    pub fn insert(&mut self, ch: char) {
        self.lines[self.i].insert(self.col, ch);
        self.col += 1;
    }

    pub fn set_line(&mut self, line: &str) {
        self.lines[self.i] = line.to_string();
        self.col = line.len();
    }

    pub fn backspace(&mut self) {
        if self.col > 0 {
            self.lines[self.i].remove(self.col - 1);
            self.col -= 1;
        }
    }

    pub fn commit(&mut self) {
        let line = self.lines.remove(self.i);
        if line.len() > 0 {
            self.lines.insert(0, line);
        }
        self.lines.insert(0, String::new());
        self.i = 0;
        self.col = 0;
    }

    pub fn clear(&mut self) {
        if self.i > 0 {
            self.i = 0;
            self.col = 0;
        } else {
            self.lines[0].clear();
            self.col = 0;
        }
    }

    pub fn home(&mut self) {
        self.col = 0;
    }

    pub fn end(&mut self) {
        self.col = self.lines[self.i].len();
    }

    pub fn up(&mut self) {
        if self.i + 1 < self.lines.len() {
            self.i += 1;
            self.col = self.lines[self.i].len();
        }
    }

    pub fn down(&mut self) {
        if self.i > 0 {
            self.i -= 1;
            self.col = self.lines[self.i].len();
        }
    }

    pub fn left(&mut self) {
        if self.col > 0 {
            self.col -= 1;
        }
    }

    pub fn right(&mut self) {
        if self.col < self.lines[self.i].len() {
            self.col += 1;
        }
    }

    pub fn render(&self, stdout: &mut termion::raw::RawTerminal<std::io::Stdout>) {
        let line = &self.lines[self.i];
        let (_x, y) = stdout.cursor_pos().unwrap();
        let x = self.col as u16 + self.prompt.len() as u16 + 1;
        write!(
            stdout,
            "\r{}{}{}{}",
            clear::CurrentLine,
            self.prompt,
            line,
            termion::cursor::Goto(x, y),
        ).unwrap();
        stdout.flush().unwrap();
    }
}

fn main() {
    let stdin = stdin();
    let mut stdout = stdout().into_raw_mode().unwrap();

    let mut editor = Editor::new();
    editor.render(&mut stdout);

    for c in stdin.events() {
        let evt = c.unwrap();
        match evt {
            Event::Key(Key::Char('\t')) => {
                if "apple".starts_with(editor.line()) {
                    editor.set_line("apple ");
                }
            },
            Event::Key(Key::Char('\n')) => {
                editor.commit();
                println!();
            },
            Event::Key(Key::Ctrl('c')) => {
                editor.clear();
                println!();
            }
            Event::Key(Key::Char(ch)) => editor.insert(ch),
            Event::Key(Key::Backspace) => editor.backspace(),
            Event::Key(Key::Ctrl('a')) => editor.home(),
            Event::Key(Key::Ctrl('e')) => editor.end(),
            Event::Key(Key::Ctrl('b')) => editor.left(),
            Event::Key(Key::Ctrl('f')) => editor.right(),
            Event::Key(Key::Ctrl('p')) => editor.up(),
            Event::Key(Key::Ctrl('n')) => editor.down(),
            Event::Key(Key::Up) => editor.up(),
            Event::Key(Key::Down) => editor.down(),
            Event::Key(Key::Left) => editor.left(),
            Event::Key(Key::Right) => editor.right(),
            Event::Key(Key::Ctrl('d')) => break,
//            e => eprintln!("{e:?}"),
            _ => (),
        }
        editor.render(&mut stdout);
    }
    println!("\r");
}
