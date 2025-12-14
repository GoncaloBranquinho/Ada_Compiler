procedure main is
    a : integer := 100;
begin
    put_line(str(a));

    declare
        a : integer := 10;
        b : integer := 20;
    begin
        put_line(str(a));
        put_line(str(b));

        declare
            a : integer := 1;
            c : integer := 2;
        begin
            put_line(str(a));
            put_line(str(b));
            put_line(str(c));
        end;

        put_line(str(a));
        put_line(str(b));
    end;

    put_line(str(a));
end main;
