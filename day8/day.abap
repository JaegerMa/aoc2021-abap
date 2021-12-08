START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/8` CHANGING input.


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

  DATA(sum) = 0.

  LOOP AT lines INTO DATA(line).

    SPLIT line AT ` | ` INTO DATA(base_words_str) DATA(code_words_str).
    SPLIT code_words_str AT ` ` INTO TABLE DATA(code_words).

    LOOP AT code_words INTO DATA(code_word).

      CASE strlen( code_word ).
        WHEN 2 OR 3 OR 4 OR 7.
          sum = sum + 1.
      ENDCASE.

    ENDLOOP.

  ENDLOOP.

  DATA(answer) = sum.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.

FORM part2.

  DATA(code_sum) = 0.
  LOOP AT lines INTO DATA(line).

    SPLIT line AT ` | ` INTO DATA(base_words_str) DATA(code_words_str).

    SPLIT base_words_str AT ` ` INTO TABLE DATA(base_words).
    SPLIT code_words_str AT ` ` INTO TABLE DATA(code_words).


    DATA chars1 TYPE string_table.
    DATA chars4 TYPE string_table.
    LOOP AT base_words INTO DATA(base_word).

      DATA(base_word_len) = strlen( base_word ).
      IF base_word_len <> 2 AND base_word_len <> 4.
        CONTINUE.
      ENDIF.

      DATA(base_word_chars) = VALUE string_table( ).
      PERFORM split_string USING base_word CHANGING base_word_chars.

      IF base_word_len = 2.
        chars1 = base_word_chars.
      ELSE.
        chars4 = base_word_chars.
      ENDIF.

    ENDLOOP.


    DATA(code) = ``.
    LOOP AT code_words INTO DATA(code_word).

      DATA code_word_chars TYPE string_table.
      DATA code_word_match_count TYPE i.
      CASE strlen( code_word ).
        WHEN 2.
          code = code && `1`.

        WHEN 3.
          code = code && `7`.

        WHEN 4.
          code = code && `4`.

        WHEN 5.

          PERFORM split_string USING code_word CHANGING code_word_chars.

          PERFORM count_matches USING chars4 code_word_chars CHANGING code_word_match_count.
          IF code_word_match_count = 2.
            code = code && `2`.
            CONTINUE.
          ENDIF.

          PERFORM count_matches USING chars1 code_word_chars CHANGING code_word_match_count.
          IF code_word_match_count = 2.
            code = code && `3`.
            CONTINUE.
          ENDIF.

          code = code && `5`.

        WHEN 6.

          PERFORM split_string USING code_word CHANGING code_word_chars.

          PERFORM count_matches USING chars1 code_word_chars CHANGING code_word_match_count.
          IF code_word_match_count = 1.
            code = code && `6`.
            CONTINUE.
          ENDIF.

          PERFORM count_matches USING chars4 code_word_chars CHANGING code_word_match_count.
          IF code_word_match_count = 3.
            code = code && `0`.
            CONTINUE.
          ENDIF.

          code = code && `9`.

        WHEN 7.
          code = code && `8`.

      ENDCASE.

    ENDLOOP.


    DATA(code_value) = CONV i( code ).
    code_sum = code_sum + code_value.

  ENDLOOP.

  DATA(answer) = code_sum.
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.


FORM split_string USING string TYPE string
               CHANGING chars TYPE string_table.

  chars = VALUE #( ).
  DO strlen( string ) TIMES.
    DATA(idx) = sy-index - 1.
    INSERT string+idx(1) INTO TABLE chars.
  ENDDO.

ENDFORM.

FORM count_matches USING base TYPE string_table
                         probe TYPE string_table
                CHANGING match_count TYPE i.

  DATA(sorted_probe) = probe.
  SORT sorted_probe.
  DELETE ADJACENT DUPLICATES FROM sorted_probe.

  match_count = REDUCE i( INIT i = 0
                          FOR char IN sorted_probe
                             NEXT i = i + COND i( WHEN line_exists( base[ table_line = char ] )
                                                  THEN 1
                                                  ELSE 0 )
                        ).

ENDFORM.
