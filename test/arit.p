program
var n : integer;
procedure coucou(i : integer) ;
var j,k,l,m : integer;
begin
   j := (((((i*i)+(i+i)+(i*i)+(i+i))+((i*i)+(i+i)+(i*i)+(i+i)))+(((i*i)+(i+i)+(i*i)+(i+i))+((i*i)+(i+i)+(i*i)+(i+i))))+((((i*i)+(i+i)+(i*i)+(i+i))+((i*i)+(i+i)+(i*i)+(i+i)))+(((i*i)+(i+i)+(i*i)+(i+i))+((i*i)+(i+i)+(i*i)+(i+i)))));
   writeln(j);
   k := j+i;
   writeln(k);
   l := j+k+i;
   writeln(l);
   m := l+j+k+i;
   writeln(m);
   writeln((((i-i)-i)-i)-i);
   writeln(i-(i-(i-(i-i))));
   writeln(i * -10);
   writeln(i / (4));
   writeln(i - (i / 4 )*4)
end;

begin
   read(n);
   while n <> 0 do begin
      coucou(n) ;
      read(n)
   end
end
;;