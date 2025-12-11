procedure Main is
  str : String := "          ";
  num : Integer := 0;
  x : Integer := 10;
begin
  Get_Line (str, num);
  x := x + num;
  if x > 12 then
    Put_Line (str);
    Put_Line(str(x));
  else
    Put_Line(str(0));
  end if;
end Main;
