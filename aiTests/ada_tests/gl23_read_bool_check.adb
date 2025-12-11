procedure Main is
  str : String := "          ";
  num : Integer := 0;
  v : Boolean := False;
begin
  Get_Line (str, num);
  if v then
    Put_Line(str(1));
  else
    Put_Line(str(0));
  end if;
end Main;
