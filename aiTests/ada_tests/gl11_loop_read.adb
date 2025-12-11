procedure Main is
  str : String := "          ";
  num : Integer := 0;
  i : Integer := 0;
begin
  while i < 2 loop
    Get_Line (str, num);
    Put_Line (str);
    i := i + 1;
  end loop;
end Main;
