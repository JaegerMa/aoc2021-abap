TYPES:
  BEGIN OF op_t,
    arg1 TYPE i,
    arg2 TYPE i,
    arg3 TYPE i,
  END OF op_t,
  op_list_t TYPE TABLE OF op_t WITH DEFAULT KEY.


START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/24` CHANGING input.


  SPLIT input AT |\n| INTO TABLE DATA(lines).
  DATA ops TYPE op_list_t.
  PERFORM read_ops CHANGING ops.

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

FORM read_ops CHANGING ops TYPE op_list_t.

  ops = VALUE #( ).

  DATA current_op TYPE REF TO op_t.
  LOOP AT lines INTO DATA(line).
    DATA(i) = sy-tabix.

    IF line = `inp w`.
      INSERT INITIAL LINE INTO TABLE ops REFERENCE INTO current_op.
    ELSEIF line(5) = `add x` AND matches( val = line regex = `^add x -?\d+$` ).
      current_op->arg1 = CONV i( line+6 ).
    ELSEIF line(5) = `div z`.
      current_op->arg2 = CONV i( line+6 ).
    ELSEIF line = `add y w`.
      DATA(next_line) = lines[ i + 1 ].
      current_op->arg3 = CONV i( next_line+6 ).
    ENDIF.

  ENDLOOP.

ENDFORM.


FORM part1.

  DATA(range) = VALUE int4_table( ( 9 ) ( 8 ) ( 7 ) ( 6 ) ( 5 ) ( 4 ) ( 3 ) ( 2 ) ( 1 ) ).

  DATA model_number TYPE int8.
  PERFORM next USING 0 0 range CHANGING model_number.

  DATA(answer) = model_number.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.
FORM part2.

  DATA(range) = VALUE int4_table( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ( 6 ) ( 7 ) ( 8 ) ( 9 ) ).

  DATA model_number TYPE int8.
  PERFORM next USING 0 0 range CHANGING model_number.

  DATA(answer) = model_number.
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.


FORM next USING z TYPE i
                model_number_pos TYPE i
                range TYPE int4_table
       CHANGING model_number TYPE int8.

  IF model_number_pos >= 14.

    IF z = 0.
      RETURN.
    ELSE.
      model_number = -1.
      RETURN.
    ENDIF.

  ENDIF.


  DATA(op) = ops[ model_number_pos + 1 ].
  LOOP AT range INTO DATA(model_number_value).

    DATA(x) = COND i( WHEN z MOD 26 + op-arg1 = model_number_value THEN 0 ELSE 1 ).
    IF op-arg2 <> 1 AND x <> 0.
      CONTINUE.
    ENDIF.


    DATA(next_z) = z DIV op-arg2 * ( 25 * x + 1 ) + ( model_number_value + op-arg3 ) * x.
    DATA(next_model_number) = model_number + CONV int8( model_number_value * ipow( base = 10 exp = 13 - model_number_pos ) ).
    DATA(next_model_number_pos) = model_number_pos + 1.

    PERFORM next USING next_z next_model_number_pos range CHANGING next_model_number.
    IF next_model_number > 0.
      model_number = next_model_number.
      RETURN.
    ENDIF.

  ENDLOOP.

  model_number = -1.

ENDFORM.
