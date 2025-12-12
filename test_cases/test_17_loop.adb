procedure main is
    s      : string;
    n      : integer;
    prefix : string;
    i      : integer := 0;
begin
    prefix := "Line:";

    while i < 3 loop
        get_line(s, n);
        put_line(prefix);
        put_line(s);
        i := i + 1;
    end loop;
end main;

