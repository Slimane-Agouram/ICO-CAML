program
var x : integer;

function times ( p : integer ;  q : integer) : integer;
begin
   times := p * q
end ;

function fact ( n : integer) : integer;
var r : integer ;
begin
   r := 1 ;
   while n > 0 do
   begin
      r := times(r,n);
      n := n-1
   end ;
   fact := r
end;

begin
   read (x);
   writeln (fact (x))
end;;
