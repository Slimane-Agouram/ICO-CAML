program
var
   max   : integer ;
   m,n,r : array of array of integer;

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

function malloc( n : integer ) : array of array of integer ;
var i,r : integer ;
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
var i,j,t,r : integer ;
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

function mmult (n : integer ; m1,m2 : array of array of integer)
  : array of array of integer;
var
   i,j,k,t : integer;
   r       : array of array of integer;
begin
   r := malloc(n) ;
   i := 0 ;
   while (i < n) do begin
      j := 0 ;
      while (j < n) do begin
         t := 0 ;
         k := 0 ;
         while (k < n) do begin
            t := t + m1[i][k] * m2[k][j] ;
            k := k+1
         end ;
         r[i][j] := t ;
         j := j+1              
      end ;         
      i := i+1 
   end ;
   mmult := r
end ;

begin
   read(max) ;
   m := mread(max) ;
   n := mread(max) ;
   affiche(max, mmult(max,m,n))
end.
