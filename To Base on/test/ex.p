program

procedure f(var t : array of integer , var n:integer);
var i : integer;
var x : integer;
   
begin
   i := n-1 ;
   while i > 0 do
   begin
      x := t[i] ;
      t[i] := t[i-1] ;
      t[i-1] := x      
   end
end;

begin
   f (alloc(101 : integer), 100)
end