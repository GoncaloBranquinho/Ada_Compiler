procedure main is
    x : integer := 1;
begin
    put_line(str(x));

    declare
        x : integer := 2;
    begin
        put_line(str(x));
    end;

    put_line(str(x));
end main;
