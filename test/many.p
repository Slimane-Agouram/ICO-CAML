program
procedure ecrit(var x  : integer);
begin
   writeln(x)
end;
procedure many (var a0 : integer, var a1 : integer, var a2 : integer,
                var a3 : integer, var a4 : integer, var a5 : integer);
begin
   ecrit (a0);
   ecrit (a1);
   ecrit (a2);
   ecrit (a3);
   ecrit (a4);
   ecrit (a5)
end;
   many (1,2,3,4,5,6)
;;
