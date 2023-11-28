use super::*;

use std::sync::Mutex;
use std::sync::mpsc::{Receiver, Sender, channel};

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;

pub struct RedrawEvent;


#[derive(Debug)]
pub struct Video {
    signal: u8,
    hsync: bool,
    vsync: bool,
    frame_buffer: String,
    frame: Arc<Mutex<[u8; 16*16*3]>>,
    index: usize,
    disabled: bool,
    initialized: bool,
    vs_sender: Sender<()>,
}

impl Video {
    pub fn new() -> Video {
        let (vs_sender, vs_receiver) = channel::<()>();

        let frame = Arc::new(Mutex::new([255u8; 16*16*3]));
        let frame2 = frame.clone();

        std::thread::spawn(move || video_thread(frame2, vs_receiver));

        Video {
            signal: 0,
            hsync: false,
            vsync: false,
            frame_buffer: String::new(),
            frame,
            index: 0,
            disabled: false,
            initialized: false,
            vs_sender,
        }
    }

    pub fn disable(&mut self) {
        self.disabled = true;
    }
}

impl ExtInstance for Video {
    fn incoming_ports(&self) -> Vec<PortName> { vec!["signal".to_string(), "hsync".to_string(), "vsync".to_string()] }

    fn update(&mut self, portname: &PortName, value: value::Value) -> Vec<(PortName, value::Value)> {
        if value.is_x() {
            return vec![];
        }
        if portname == "signal" {
            self.signal = value.try_into().unwrap();
        } else if portname == "hsync" {
            self.hsync = value.try_into().unwrap();
        } else if portname == "vsync" {
            self.vsync = value.try_into().unwrap();
        }
        vec![]
    }

    fn clock(&mut self) -> Vec<(PortName, Value)> {
        let v = match self.signal {
            0 => 0,
            1 => 128,
            2 => 178,
            _ => 255,
        };
        {
            let mut frame = self.frame.lock().unwrap();
            frame[self.index] = v as u8;
            frame[self.index + 1] = v as u8;
            frame[self.index + 2] = v as u8;
        }
        if self.index+3 < 16 * 16 * 3 {
            self.index += 3;
        } else {
            self.index = 0;
        }

        if self.vsync {
            self.vs_sender.send(()).unwrap();
        }
        vec![]
    }

    fn reset(&mut self) -> Vec<(PortName, Value)>{
//        if !self.disabled {
//            if !self.initialized {
//                ctrlc::set_handler(move || {
//                    // show the cursor
//                    println!("\x1B[?25h");
//                    std::process::exit(0);
//                }).unwrap();
//            }
//
//            // hide the cursor
//            print!("\x1B[?25l");
//
//            // clear the screen
//            print!("\x1B[2J");
//        }

        self.initialized = true;

        vec![]
    }
}

fn video_thread(frame: Arc<Mutex<[u8; 16*16*3]>>, vs_receiver: Receiver<()>) {
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem
        .window("Nettle Simulator", 512, 512)
        .position_centered()
        .resizable()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();

    let texture_creator = canvas.texture_creator();
    let mut surface = sdl2::surface::Surface::new(
        16, // Width
        16, // Height
        sdl2::pixels::PixelFormatEnum::RGB24,
    ).unwrap();


    canvas.set_draw_color(Color::RGB(255, 255, 255));
    canvas.clear();
    canvas.present();

    sdl_context.event().unwrap().register_custom_event::<RedrawEvent>().unwrap();

    let event_subsystem = sdl_context.event().unwrap();

    let timer = sdl_context.timer().unwrap();
    let _t = timer.add_timer(200, Box::new(|| {
        let redraw_event = RedrawEvent;
        event_subsystem.push_custom_event(redraw_event).unwrap();
        0 // Repeat the timer after the interval
    }));

    let mut event_pump = sdl_context.event_pump().unwrap();

    loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => std::process::exit(0),
                Event::User { .. } => {
                    vs_receiver.recv().unwrap();
                    {
                        let frame = frame.lock().unwrap();
                        let bytes = surface.without_lock_mut().unwrap();
                        bytes.copy_from_slice(&*frame);
                    }
                    let texture = texture_creator.create_texture_from_surface(&surface).unwrap();
                    canvas.copy(&texture, None, None).unwrap();
                    canvas.present();
                    event_subsystem.push_custom_event(RedrawEvent).unwrap();
                },
                _ => (),
                //e => println!("{e:?}"),
            }
        }
    }
}
