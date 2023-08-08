use std::{
    io::{BufRead, BufReader, Read, Write},
    process::{Child, Command, Stdio},
    sync::mpsc::{channel, Receiver, Sender, TryRecvError},
    thread,
};

pub struct Process {
    pub stdin_sender: Sender<String>,
    pub stdout_receiver: Receiver<String>,
    pub stderr_receiver: Receiver<String>,
    pub child: Child,
}

impl Process {
    pub fn new<S: AsRef<std::ffi::OsStr>>(program: S) -> Self {
        let (stdin_sender, stdin_receiver) = channel::<String>();
        let (stdout_sender, stdout_receiver) = channel();
        let (stderr_sender, stderr_receiver) = channel();

        let mut child = Command::new(program)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("could not start..");

        fn create_out_thread<T>(out: T, sender: Sender<String>)
        where
            T: Read + Send + 'static,
        {
            thread::spawn(move || {
                let reader = BufReader::new(out);
                for line in reader.lines() {
                    match line {
                        Ok(line) => {
                            if sender.send(line).is_err() {
                                break;
                            }
                        }
                        Err(err) => {
                            dbg!(err);
                        }
                    }
                }
            });
        }

        create_out_thread(child.stdout.take().unwrap(), stdout_sender);
        create_out_thread(child.stderr.take().unwrap(), stderr_sender);

        let mut stdin = child.stdin.take().unwrap();
        thread::spawn(move || {
            while let Ok(line) = stdin_receiver.recv() {
                if stdin.write_all(line.as_bytes()).is_err() {
                    break;
                }
            }
        });

        Self {
            stdin_sender,
            stdout_receiver,
            stderr_receiver,
            child,
        }
    }

    fn get_stdx_line(receiver: &Receiver<String>) -> Option<String> {
        match receiver.try_recv() {
            Ok(line) => Some(line),
            Err(err) => {
                if matches!(err, TryRecvError::Empty) {
                    None
                } else {
                    panic!("{err}");
                }
            }
        }
    }

    pub fn get_stdout_line(&self) -> Option<String> {
        Self::get_stdx_line(&self.stdout_receiver)
    }

    pub fn get_stderr_line(&self) -> Option<String> {
        Self::get_stdx_line(&self.stderr_receiver)
    }

    pub fn send_stdin_line(&self, line: &str) {
        self.stdin_sender.send(line.to_owned() + "\n").unwrap();
    }
}
