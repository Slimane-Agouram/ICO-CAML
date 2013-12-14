program
function sum (var n : integer) : integer;
     begin
       if n > 0 then sum := n + sum (n-1) else sum := 0
     end;

begin
   sum(10)
end ;;