procedure main is
    name : string;
    n    : integer;
    msg  : string;
begin
    get_line(name, n);
    msg := "Hello " & name & "!";
    put_line(msg);
end main;

