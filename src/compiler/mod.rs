use parser::Parser;
use ir_generator::IRGen;
use assembler::Assembler;
use assembler::types::ByteCodeVec;
use std::str::Chars;
use std::io::prelude::*;
use std::path::Path;
use std::fs::File;
use std::error::Error;

#[derive(Debug)]
pub struct Compiler {

}

impl Compiler {
    pub fn from_string(text: &String, source_name: &String) -> ByteCodeVec {
        let ast = Parser::<Chars>::ast_from_text(text).expect("Syntax Error");
        let mut ir_gen = IRGen::new();
        ir_gen.generate_ir(&ast).expect("Generating IR failed");

        println!("{:?}", ir_gen.get_chunk());

        let bytecode = Assembler::assemble(ir_gen.get_chunk(), source_name)
            .expect("Assemble failed");
        bytecode
    }

    pub fn from_file(path_str: &String) -> ByteCodeVec {
        let path = Path::new(path_str);
        let mut file = match File::open(&path) {
            Ok(f) => f,
            Err(why) => panic!("Could not open {}: {}", path.display(), why.description()),
        };

        let mut source = String::new();
        file.read_to_string(&mut source).expect("Read failed.");
        Compiler::from_string(&source,
                              &String::from(path.file_name()
                                  .unwrap()
                                  .to_str()
                                  .unwrap()
                                  .to_string()))
    }
}

mod tests {
    use super::*;
    use std::process::Command;
    use std::path::Path;
    use std::env;
    use std::fs::{File, OpenOptions};

    static BC_FILE_LOCATION: &'static str = "src/test_cases";
    static mut WORD_DIR_SET: bool = false;

    fn write_bytecode(name: &String, bytecode: ByteCodeVec) {
        let path = Path::new(name);
        let mut file = match File::create(&path) {
            Err(why) => panic!("Could not create {}: {}", path.display(), why.description()),
            Ok(f) => f,
        };
        match file.write_all(&bytecode) {
            Err(why) => {
                panic!("Could not write to {}: {}",
                       path.display(),
                       why.description())
            }
            Ok(_) => {}
        };
        file.sync_all().expect("Sync failed.");
    }

    fn setup_dir() {
        unsafe {
            if !WORD_DIR_SET {
                let wdr = env::current_dir().unwrap().join(BC_FILE_LOCATION);
                assert!(env::set_current_dir(&wdr).is_ok());
            }
            WORD_DIR_SET = true;
        }
    }

    fn run_and_check(name: &String, bytecode: ByteCodeVec, expect: &str) {
        setup_dir();
        write_bytecode(name, bytecode);

        let output = Command::new("lua").arg(name).output().expect("Could not run lua.");
        let expect = expect.to_string();
        println!("Running: {:?}", name);
        assert_eq!(output.stdout, expect.into_bytes());
    }

    #[test]
    fn simple_io() {
        let code = "print(1, 2)\n".to_string();
        let name = "simple_io".to_string();
        let bytecode = Compiler::from_string(&code, &name);
        run_and_check(&name, bytecode, "1\t2\r\n");

    }

    #[test]
    fn simple_int_arith() {
        let code = "\
            local a = 3 * (1 + 1)  --6
            local b = 10 / (2 + 3)  --2
            print(a + b, b - a)
        "
            .to_string();
        let name = "simple_int_arith".to_string();
        let bytecode = Compiler::from_string(&code, &name);
        run_and_check(&name, bytecode, "8\t-4\r\n");
    }

    #[test]
    fn branch() {
        let code = "\
            local a, b = 1, 2
            local cond = true

            if a ~= b then
                c = 2
            else 
                c = 3
            end
            if cond then
                d = 4
            else 
                d = 5
            end
            print(c, d)
        "
            .to_string();
        let name = "branch".to_string();
        let bytecode = Compiler::from_string(&code, &name);
        run_and_check(&name, bytecode, "2\t4\r\n");
    }

    #[test]
    fn while_loop() {
        let code = "\
            local i, sum = 0, 0
            while i <= 100 do
                sum = sum + i
                i = i + 1
            end
            print(sum)
        "
            .to_string();
        let name = "while_loop".to_string();
        let bytecode = Compiler::from_string(&code, &name);
        run_and_check(&name, bytecode, "5050\r\n");
    }
}