procedure Main is
  str : String := "          ";
  num : Integer := 0;
begin
  Get_Line (str, num);
  num := num ** 2;
  Put_Line(str(num));
end Main;
