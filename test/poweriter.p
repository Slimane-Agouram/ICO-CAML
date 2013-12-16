program
var
   x,n : integer ;

function power(x,n : integer) : integer ;
var r,t : integer ;
begin
   t := x ; r := 1 ;
   while n > 0 do begin
      if n <> 2*(n/2) then
         r := r * t
      else begin
      end ;
      n := n / 2 ;
      t := t*t
   end ;
   power := r
end ;

begin
   read(x) ; read(n) ;
   writeln(power(x,n))
end;;