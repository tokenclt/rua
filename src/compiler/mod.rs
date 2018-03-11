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
    pub fn from_string(text: &String, source_name: &str) -> ByteCodeVec {
        let ast = Parser::<Chars>::ast_from_text(text).expect("Syntax Error");
        let mut ir_gen = IRGen::new();
        ir_gen.generate_ir(&ast).expect("Generating IR failed");

        //println!("{:?}", ir_gen.get_chunk(source_name));

        let bytecode = Assembler::assemble(ir_gen.get_chunk(source_name))
            .expect("Assemble failed");
        bytecode
    }

    pub fn from_file(path_str: &str) -> ByteCodeVec {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::process::Command;
    use std::fs::File;
    use tempdir::TempDir;


    fn run_and_check(name: &String, bytecode: ByteCodeVec, expect: &str) {

        let tmp_dir = TempDir::new("rua").expect("Failed to create temp dir");
        let file_path = tmp_dir.path().join(name);

        let mut file = File::create(&file_path).expect("Could not create bytecode file");
        file.write_all(&bytecode).expect("Failed to write bytecode");
        file.sync_all().expect("Faild to flush");

        let output = Command::new("lua").arg(file_path).output().expect("Could not run lua.");
        let expect = expect.to_string();
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
    fn boolean_arith() {
        let code = "
            local a, b = true, false
            local c = not ( 3 <= 2 or a == b)
            print(c)
        "
            .to_string();
        let name = "boolean_arith".to_string();
        let bc = Compiler::from_string(&code, &name);
        run_and_check(&name, bc, "true\r\n");
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

    #[test]
    fn for_numeric() {
        let code = "\
            local sum1, sum2 = 0, 0
            for i = 1, 100, 1 do
                sum1 = sum1 + i
            end
            for i = 0, 100, 2 do
                sum2 = sum2 + i 
            end
            print(sum1, sum2)
        "
            .to_string();
        let name = "for_numeric".to_string();
        let bc = Compiler::from_string(&code, &name);
        run_and_check(&name, bc, "5050\t2550\r\n");
    }

    #[test]
    fn build_table() {
        let code = "\
            table = {name='Ann', age = 10 + 8,
                    1, 2, 3}
            print(table.name, table['age'], table[1], table[2], table[3])
        "
            .to_string();
        let name = "build_table".to_string();
        let bc = Compiler::from_string(&code, &name);
        run_and_check(&name, bc, "Ann\t18\t1\t2\t3\r\n");
    }

    #[test]
    fn func_play() {
        let code = "\
            add_sub = function(a, b)
                return a + b, a - b
            end
            print(add_sub(1, 2))
        "
            .to_string();
        let name = "func_play".to_string();
        let bc = Compiler::from_string(&code, &name);
        run_and_check(&name, bc, "3\t-1\r\n");
    }
    // FIXME: Parser do not report error for
    // function(a, b)
    // a + b
    // end
    //

    // FIXME: Resource allocator hierachy is bad
    //        can't go back to parent level
    //        consider merging symbol table as allocator

}