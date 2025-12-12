procedure main is
    a : integer := 5;
    b : integer := 10;
    c : integer := 15;
    cond : boolean;
begin
    cond := (a < b and b < c) or (c = 0) xor false;
    if cond then
        put_line("True");
    else
        put_line("False");
    end if;
end main;
