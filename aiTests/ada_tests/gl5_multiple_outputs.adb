procedure Main is
  str : String := "          ";
  num : Integer := 0;
begin
  Put_Line ("start");
  Get_Line (str, num);
  Put_Line (str);
  Put_Line(str(num));
  Put_Line ("end");
end Main;
