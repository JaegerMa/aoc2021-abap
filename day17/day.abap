START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/17` CHANGING input.
  PERFORM main.


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


FORM main.

  FIND REGEX 'target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)' IN input SUBMATCHES DATA(minx_str) DATA(maxx_str) DATA(miny_str) DATA(maxy_str).

  DATA(min_x) = CONV i( minx_str ).
  DATA(max_x) = CONV i( maxx_str ).
  DATA(min_y) = CONV i( miny_str ).
  DATA(max_y) = CONV i( maxy_str ).

  DATA(highest_y) = 0.
  DATA(successful_shots_count) = 0.

  DATA(x) = 1.

  "Find the lowest value the start velocity X
  "has to be to reach the target.
  "That is the lowest value whose factorial is >= min_x.
  WHILE x * ( x + 1 ) / 2 < min_x.
    x = x + 1.
  ENDWHILE.

  WHILE x <= max_x.

    DATA(y) = min_y.
    WHILE y < 1000.

      DATA(target_hit) = abap_false.
      DATA(bullet_x) = 0.
      DATA(bullet_y) = 0.
      DATA(velocity_x) = x.
      DATA(velocity_y) = y.
      DATA(current_highest_y) = 0.

      WHILE bullet_x <= max_x AND bullet_y >= min_y.

        bullet_x = bullet_x + velocity_x.
        bullet_y = bullet_y + velocity_y.

        current_highest_y = nmax( val1 = current_highest_y val2 = bullet_y ).

        IF velocity_x > 0.
          velocity_x = velocity_x - 1.
        ENDIF.
        velocity_y = velocity_y - 1.


        IF bullet_x >= min_x AND bullet_x <= max_x AND bullet_y >= min_y AND bullet_y <= max_y.
          highest_y = nmax( val1 = highest_y val2 = current_highest_y ).
          successful_shots_count = successful_shots_count + 1.
          target_hit = abap_true.
          EXIT.
        ENDIF.

      ENDWHILE.

      "We overshot the target. There's no sense in trying higher Y values,
      "so let's move on with the next X.
      IF target_hit = abap_false AND bullet_y >= min_y.
        EXIT.
      ENDIF.

      y = y + 1.
    ENDWHILE.

    x = x + 1.
  ENDWHILE.


  WRITE / 'Part 1:'.
  WRITE CONV string( highest_y ).
  WRITE / 'Part 2:'.
  WRITE CONV string( successful_shots_count ).

ENDFORM.
