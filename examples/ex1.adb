procedure Main is
  z : Integer;
  p : String;
  k : String := "oSASASa";
  l : String := "EE";
begin
  Get_Line(k,z);
  put_line(k);
  k := "ola" & "adeus" & "sim";
  l := k & "sim" & "adeus" & "ola";
  k := l;
  put_line(k);
end Main;

