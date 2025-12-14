procedure main is
    x : integer := 10;
begin
    put_line(str(x));

    declare
        y : integer := 20;
    begin
        put_line(str(x));
        put_line(str(y));
    end;

    put_line(str(x));
end main;
