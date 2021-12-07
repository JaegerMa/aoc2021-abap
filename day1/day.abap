START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/1` CHANGING input.


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

  DATA(count) = 0.
  LOOP AT lines INTO DATA(line) FROM 2.

    IF CONV i( line ) > CONV i( lines[ sy-tabix - 1 ] ).
      count = count + 1.
    ENDIF.

  ENDLOOP.


  DATA(answer) = count.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.

FORM part2.

  DATA(count) = 0.
  LOOP AT lines INTO DATA(line) FROM 4.

    IF CONV i( line ) > CONV i( lines[ sy-tabix - 3 ] ).
      count = count + 1.
    ENDIF.

  ENDLOOP.


  DATA(answer) = count.
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.
