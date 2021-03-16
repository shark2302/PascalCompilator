import os
import sys

from compiler import program


def main() -> None:
    prog1 = '''
        int input_int(string name) begin
            if (name != "") begin
                print("Введите " + name + ": ");
            end
            return to_int(read());
        end
        float input_float(string name) begin
            if (name != "") begin
                print("Введите " + name + ": ");
            end
            return to_float(read());
        end

        int g, g2 := g, g4 := 90;

        int a := input_int("a");
        float b := input_float("b"), c := input_float("c");  /* comment 1
        int d := input_int("d");
        */
        for (int i := 0, j := 8; ((i <= 5)) && g; i := i + 1, print(5))
            for(; a < b;)
                if (a > 7 + b) begin
                c := a + b * (2 - 1) + 0;  // Падает здесь, если раскомментить
                    string bb := "98\tура";
                end
                else if (a)
                    print((c + 1) + " " + 89.89);
        for(bool i := true;;);

        int z;
        z:=0;
    '''
    prog2 = 'int f1(int p1, float p2) begin string a := p1 + p2; int x; end'''
    prog3 = 'for (;;);'
    prog4 = 'int i; i := 5;'
    prog5 = '''
        int input_int(string name) begin
            if (name != "") begin
                print("Введите " + name + ": ");
            end
            return to_int(read());

            // bool a() begin end
        end
        int input_int2(string name, int a, int name2) begin
            if (name != "") begin
                print("Введите " + name + ": ");
            end
            return "";
        end
    '''
    prog6 = """int a := 6;"""
    program.execute(prog1)


if __name__ == "__main__":
    main()
