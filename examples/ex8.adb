procedure Main is
  z : Float;
  a : Integer;
  p : String;
  k : String := "oSASASa";
  l : String := "EE";
begin
  Get_Line(k, a);
  put_line(k);
  k := "ola" & "adeus" & "sim";
  l := k & "sim" & "adeus" & "ola";
  k := l;
  put_line(k);
  z := 3.4234174**21;
  p := str(z);
  p := p & "ola";
  put_line(p);
end Main;
