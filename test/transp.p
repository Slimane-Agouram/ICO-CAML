program
var
   max : integer ;
   m   : array of array of integer;


procedure affiche (n : integer ; p :  array of array of integer) ;
var i,j : integer ;
begin
   i := 0 ;
   while (i < n) do begin
      j := 0 ;
      while (j < n) do begin
         writeln(p[i][j]) ;
         j := j+1              
      end ;         
      i := i+1 
   end 
end ;

function malloc(n : integer ) : array of array of integer ;
var
   i : integer ;
   r : array of array of integer;
begin
   r := alloc(n : array of integer) ;
   i := 0 ;
   while (i < n) do begin
      r[i] := alloc(n : integer) ;
      i := i+1 
   end  ;
   malloc := r
end ;

function mread(n : integer ) : array of array of integer ;
var
   i,j,t : integer ;
   r     : array of array of integer ;
begin
   r := malloc(n) ;
   i := 0 ;
   while (i < n) do begin
      j := 0 ;
      while (j < n) do begin
         read(t) ;
         r[i][j] := t ;
         j := j+1              
      end ;         
      i := i+1 
   end ;
   mread := r
end ;

procedure trans(n : integer ; m : array of array of integer) ;
var i,j,t : integer ;
begin
   i := n-1 ;
   while i >= 0 do begin
      j := n-1 ;
      while j > i do begin
         t := m[i][j] ;
         m[i][j] := m[j][i] ;
         m[j][i] := t ;
         j := j-1
      end ;
      i := i-1
   end
end;

begin
   read(max) ;
   m := mread(max) ;
   trans(max, m) ;
   affiche(max, m)
end;;
