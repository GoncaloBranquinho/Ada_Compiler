procedure Main is
  z : Integer;
  p : String;
  k : String := "oSASASa";
  l : String := "EE";
begin
  Get_Line(k,z);
  put_line(k);
  l := "ola" & "adeus" & "sim";
  k := l & "sim" & "adeus" & "ola" & k;
  l := k;
  put_line(l);
end Main;

