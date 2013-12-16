program
var x : integer;
function times (var p : integer, var q : integer) : integer;
times := p * q;

function fact (var n : integer) : integer;
var r : integer ;
begin
   r := 1 ;
   while n > 0 do
   begin
      r := times(r,n);
      n := n+n-n-1      
   end ;
   fact := r
end;

begin
   read (x);
   writeln (fact (x))
end;;
