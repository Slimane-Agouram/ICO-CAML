program
var
   x : integer;

function times ( p : integer ; q : integer) : integer;
begin
   times := p * q
end ;

function fact (n : integer) : integer;
begin
   if n <= 1 then
      fact := 1
   else
      fact := times (n, fact (n - 1))
end ;

begin
   read (x);
   writeln (fact (x))
end.
