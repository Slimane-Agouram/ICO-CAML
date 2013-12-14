program
var
   x : integer;

function fact (n : integer ; r : integer) : integer;
begin
   if n <= 1 then fact := r else fact := fact (n-1,n * r)
end ;

begin
   read (x);
   writeln (fact (x,1))
end.