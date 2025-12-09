procedure Main is
  x, y : String;
begin
  x := "1";
  y := "2";
  put_line(x);
  put_line(y);
  declare
    x, z : String;
  begin
    x := "3";
    z := "4";
    put_line(x);
    put_line(z);
  end;
  put_line(x);
  put_line(y);
end Main;
