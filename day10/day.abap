START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/10` CHANGING input.


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

  DATA(total_score) = 0.
  LOOP AT lines INTO DATA(line).

    DATA(score) = 0.
    DATA(cursor) = 0.
    WHILE cursor < strlen( line ) AND score = 0.
      PERFORM open_corrupt USING line CHANGING cursor score.
    ENDWHILE.

    total_score = total_score + score.

  ENDLOOP.


  DATA(answer) = total_score.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.
FORM open_corrupt USING line TYPE string
               CHANGING cursor TYPE i
                        score TYPE i.

  DATA(open_char) = line+cursor(1).
  DATA(close_char) = SWITCH string( open_char
                                    WHEN `(` THEN `)`
                                    WHEN `[` THEN `]`
                                    WHEN `{` THEN `}`
                                    WHEN `<` THEN `>` ).
  cursor = cursor + 1.


  WHILE cursor < strlen( line ).

    DATA(current_char) = line+cursor(1).
    CASE current_char.
      WHEN `(`
        OR `[`
        OR `{`
        OR `<`.
        PERFORM open_corrupt USING line CHANGING cursor score.

      WHEN `)`.
        cursor = cursor + 1.
        IF current_char <> close_char.
          score = score + 3.
        ENDIF.

        RETURN.

      WHEN `]`.
        cursor = cursor + 1.
        IF current_char <> close_char.
          score = score + 57.
        ENDIF.

        RETURN.

      WHEN `}`.
        cursor = cursor + 1.
        IF current_char <> close_char.
          score = score + 1197.
        ENDIF.

        RETURN.

      WHEN `>`.
        cursor = cursor + 1.
        IF current_char <> close_char.
          score = score + 25137.
        ENDIF.

        RETURN.

    ENDCASE.

  ENDWHILE.

ENDFORM.


FORM part2.

  DATA total_scores TYPE TABLE OF int8.
  DATA(total_score) = 0.
  LOOP AT lines INTO DATA(line).

    DATA(score) = VALUE int8( ).
    DATA(cursor) = 0.
    DATA(corrupt) = abap_false.
    WHILE cursor < strlen( line ) AND corrupt = abap_false.
      PERFORM open_incomplete USING line CHANGING cursor score corrupt.
    ENDWHILE.

    IF corrupt = abap_true.
      CONTINUE.
    ENDIF.
    IF score = 0.
      CONTINUE.
    ENDIF.

    INSERT score INTO TABLE total_scores.

  ENDLOOP.


  SORT total_scores.

  DATA(answer) = total_scores[ ( lines( total_scores ) + 1 ) DIV 2 ].
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.
FORM open_incomplete USING line TYPE string
                  CHANGING cursor TYPE i
                           score TYPE int8
                           corrupt TYPE sap_bool.

  DATA(open_char) = line+cursor(1).
  DATA(close_char) = SWITCH string( open_char
                                    WHEN `(` THEN `)`
                                    WHEN `[` THEN `]`
                                    WHEN `{` THEN `}`
                                    WHEN `<` THEN `>` ).
  cursor = cursor + 1.


  WHILE corrupt = abap_false.

    IF cursor >= strlen( line ).
      score = score * 5.
      score = score + SWITCH int8( open_char
                                WHEN `(` THEN 1
                                WHEN `[` THEN 2
                                WHEN `{` THEN 3
                                WHEN `<` THEN 4 ).
      RETURN.
    ENDIF.

    DATA(current_char) = line+cursor(1).
    CASE current_char.
      WHEN `(`
        OR `[`
        OR `{`
        OR `<`.
        PERFORM open_incomplete USING line CHANGING cursor score corrupt.

      WHEN `)`.
        cursor = cursor + 1.
        IF current_char <> close_char.
          corrupt = abap_true.
        ENDIF.

        RETURN.

      WHEN `]`.
        cursor = cursor + 1.
        IF current_char <> close_char.
          corrupt = abap_true.
        ENDIF.

        RETURN.

      WHEN `}`.
        cursor = cursor + 1.
        IF current_char <> close_char.
          corrupt = abap_true.
        ENDIF.

        RETURN.

      WHEN `>`.
        cursor = cursor + 1.
        IF current_char <> close_char.
          corrupt = abap_true.
        ENDIF.

        RETURN.

    ENDCASE.

  ENDWHILE.

ENDFORM.
