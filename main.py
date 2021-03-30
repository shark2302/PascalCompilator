import os
import sys

from compiler import program


def main() -> None:
    prog1 = '''
        int input_int(string name) {
            if (name != "") {
                print("Введите " + name + ": ");
            }
            return to_int(read());
        }
        float input_float(string name) {
            if (name != "") {
                print("Введите " + name + ": ");
            }
            return to_float(read());
        }

        int g, g2 = g, g4 = 90;

        int a = input_int("a");
        float b = input_float("b"), c = input_float("c");  /* comment 1
        int d = input_int("d");
        */
        for (int i = 0, j = 8; ((i <= 5)) && g; i = i + 1, print(5))
            for(; a < b;)
                if (a > 7 + b) {
                    c = a + b * (2 - 1) + 0;  // comment 2
                    string bb = "98\tура";
                }
                else if (a)
                    print((c + 1) + " " + 89.89);
        for(bool i = true;;);

        int z;
        z=0;
    '''
    prog2 = 'int f1(int p1, float p2) { string a = p1 + p2; int x; }'''
    prog3 = 'for (;;);'
    prog4 = 'int i; i = 5;'
    prog5 = '''
        int input_int(string name) {
            if (name != "") {
                print("Введите " + name + ": ");
            }
            return to_int(read());

            // bool a() { }
        }
        int input_int2(string name, int a, int name2) {
            if (name != "") {
                print("Введите " + name + ": ");
            }
            return "";
        }
    '''

    prog6 = '''
        var a, b: int; s: string;
        a := 5;
        b := 3 + a;

        while a < 10 do
        begin
        a := a + 1;
        end
        
    '''

    program.execute(prog6)


if __name__ == "__main__":
    main()
