START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/2` CHANGING input.


  SPLIT input AT |\n| INTO TABLE DATA(lines).

  PERFORM part1.
  PERFORM part2.



FORM load_input USING path TYPE string
             CHANGING input TYPE string.

  DATA input_bin TYPE xstring.
  OPEN DATASET path FOR INPUT IN BINARY MODE.
  READ DATASET path INTO input_bin.
  CLOSE DATASET path.

  DATA(line_break) = CONV xstring( '0A' ).
  DATA(zero) = CONV xstring( '00' ).
  SHIFT input_bin RIGHT DELETING TRAILING line_break IN BYTE MODE.
  SHIFT input_bin LEFT DELETING LEADING zero IN BYTE MODE.

  input = cl_http_utility=>if_http_utility~decode_utf8( input_bin ).

ENDFORM.

FORM part1.

  DATA(x) = 0.
  DATA(y) = 0.
  LOOP AT lines INTO DATA(line).

    SPLIT line AT ` ` INTO DATA(command) DATA(param1_str).
    DATA(param1) = CONV i( param1_str ).

    CASE command.
      WHEN 'forward'.
        x = x + param1.
      WHEN 'down'.
        y = y + param1.
      WHEN 'up'.
        y = y - param1.
    ENDCASE.

  ENDLOOP.


  DATA(answer) = x * y.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.

FORM part2.

  DATA(x) = 0.
  DATA(y) = 0.
  DATA(aim) = 0.
  LOOP AT lines INTO DATA(line).

    SPLIT line AT ` ` INTO DATA(command) DATA(param1_str).
    DATA(param1) = CONV i( param1_str ).

    CASE command.
      WHEN 'forward'.
        x = x + param1.
        y = y + ( aim * param1 ).
      WHEN 'down'.
        aim = aim + param1.
      WHEN 'up'.
        aim = aim - param1.
    ENDCASE.

  ENDLOOP.


  DATA(answer) = x * y.
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.
