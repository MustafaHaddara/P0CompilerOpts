from P0 import compileString

# compileString("""program p;
#   type T = record d,e:integer end;
#   var c: integer;
#   var b: T;
#   begin 
#     b.e := 10;
#     b.d := 5
#     {write(c)}
#   end
# """, target='opt')
target = 'opt' if 1 else 'mips'
compileString("""program p;
      type T = record d,e,f:integer end;
      type A = array[0..9] of integer;
      var c: integer;
      var b: boolean;
      var w: A;
      procedure q(var z: integer);
            type T = record d,e:integer end;
            type A = array[0..9] of integer;
            var y: integer;
            var m,n: boolean;
            var w: T;
            var wd, we: integer;

            begin b:=true; w.d := 9; w.e := 5; w.d := 3 - 1 end;
      begin 
      w[1] := 9; w[2] := 5;
        c := 10;
        c := c + 1;
        write(c);
        q(c)
      end
    """, target=target)

    

# compileString("""program p;
#   var c: integer;
#   var b: integer;
#     procedure q(var z: boolean);
#         type T = record d,e:integer end;
#         {var y: integer;}
#         {var m,n: boolean;}
#         var w: T;
#         {procedure p(var x: integer);
#             begin x := 10; x := x + 1 end;}
#         begin w.d := 3; w.e := 4; z := true end;
#   begin 
#     c := 10;
#     c := c + 1;
#     write(c)
#   end
# """, target='opt')