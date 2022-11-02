TYPES:
  BEGIN OF instruction_t,
    state TYPE i,
    x1    TYPE i,
    x2    TYPE i,
    y1    TYPE i,
    y2    TYPE i,
    z1    TYPE i,
    z2    TYPE i,
  END OF instruction_t,
  instruction_list_t TYPE STANDARD TABLE OF instruction_t WITH DEFAULT KEY.

TYPES:
  BEGIN OF box_t,
    x1 TYPE i,
    x2 TYPE i,
    y1 TYPE i,
    y2 TYPE i,
    z1 TYPE i,
    z2 TYPE i,
  END OF box_t,
  boxes_t TYPE STANDARD TABLE OF box_t WITH DEFAULT KEY.


START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/22` CHANGING input.

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

  DATA instructions TYPE TABLE OF instruction_t.
  PERFORM read_instruction_list USING lines CHANGING instructions.

  DATA total_overlap TYPE int8.
  DATA boxes TYPE boxes_t.

  DO lines( instructions ) TIMES.

    DATA(instruction_idx) = lines( instructions ) - sy-index + 1.
    DATA(instruction) = REF #( instructions[ instruction_idx ] ).
    IF abs( instruction->x1 ) > 50
    OR abs( instruction->x2 ) > 50
    OR abs( instruction->y1 ) > 50
    OR abs( instruction->y2 ) > 50
    OR abs( instruction->z1 ) > 50
    OR abs( instruction->z2 ) > 50.
      CONTINUE.
    ENDIF.

    DATA(box) = CORRESPONDING box_t( instruction->* ).


    IF instruction->state = 1.
      DATA(volume) = CONV int8( 0 ).
      DATA(overlap) = CONV int8( 0 ).
      PERFORM get_volume USING box CHANGING volume.
      PERFORM get_overlap USING box boxes 1 CHANGING overlap.
      total_overlap = total_overlap + volume - overlap.
    ENDIF.

    INSERT box INTO TABLE boxes.

  ENDDO.


  DATA(answer) = total_overlap.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.

FORM part2.

  DATA instructions TYPE TABLE OF instruction_t.
  PERFORM read_instruction_list USING lines CHANGING instructions.

  DATA total_overlap TYPE int8.
  DATA boxes TYPE boxes_t.

  DO lines( instructions ) TIMES.

    DATA(instruction_idx) = lines( instructions ) - sy-index + 1.
    DATA(instruction) = REF #( instructions[ instruction_idx ] ).
    DATA(box) = CORRESPONDING box_t( instruction->* ).


    IF instruction->state = 1.
      DATA(volume) = CONV int8( 0 ).
      DATA(overlap) = CONV int8( 0 ).
      PERFORM get_volume USING box CHANGING volume.
      PERFORM get_overlap USING box boxes 1 CHANGING overlap.
      total_overlap = total_overlap + volume - overlap.
    ENDIF.

    INSERT box INTO TABLE boxes.

  ENDDO.


  DATA(answer) = total_overlap.
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.

FORM read_instruction USING instruction_str TYPE string
                      CHANGING instruction TYPE instruction_t.

  SPLIT instruction_str AT ` ` INTO DATA(state_str) DATA(pos_str).
  SPLIT pos_str AT `,` INTO DATA(pos_x_str) DATA(pos_y_str) DATA(pos_z_str).
  SPLIT pos_x_str AT `..` INTO DATA(pos_x1_str) DATA(pos_x2_str).
  SPLIT pos_y_str AT `..` INTO DATA(pos_y1_str) DATA(pos_y2_str).
  SPLIT pos_z_str AT `..` INTO DATA(pos_z1_str) DATA(pos_z2_str).

  pos_x1_str = pos_x1_str+2.
  pos_y1_str = pos_y1_str+2.
  pos_z1_str = pos_z1_str+2.

  instruction = VALUE #(
    state = COND #( WHEN state_str = 'on' THEN 1 ELSE 0 )
    x1 = CONV #( pos_x1_str )
    x2 = CONV #( pos_x2_str )
    y1 = CONV #( pos_y1_str )
    y2 = CONV #( pos_y2_str )
    z1 = CONV #( pos_z1_str )
    z2 = CONV #( pos_z2_str )
  ).

ENDFORM.
FORM read_instruction_list USING instruction_list_str TYPE string_table
                        CHANGING instruction_list TYPE instruction_list_t.

  instruction_list = VALUE #( ).

  DATA instructions TYPE TABLE OF instruction_t.
  DATA instruction TYPE instruction_t.
  LOOP AT instruction_list_str INTO DATA(line).
    PERFORM read_instruction USING line CHANGING instruction.
    INSERT instruction INTO TABLE instruction_list.
  ENDLOOP.

ENDFORM.


FORM get_volume USING box TYPE box_t
             CHANGING volume TYPE int8.

  volume = ( box-x2 - box-x1 + 1 ) * ( box-y2 - box-y1 + 1 ) * ( box-z2 - box-z1 + 1 ).

ENDFORM.
FORM get_overlap USING box TYPE box_t
                       boxes TYPE boxes_t
                       overlap_start TYPE int8
              CHANGING overlap TYPE int8.

  overlap = 0.

  LOOP AT boxes INTO DATA(b2) FROM overlap_start.

    DATA(box_idx) = sy-tabix.

    DATA(overlap_min_x) = nmax( val1 = box-x1 val2 = b2-x1 ).
    DATA(overlap_max_x) = nmin( val1 = box-x2 val2 = b2-x2 ).
    DATA(overlap_min_y) = nmax( val1 = box-y1 val2 = b2-y1 ).
    DATA(overlap_max_y) = nmin( val1 = box-y2 val2 = b2-y2 ).
    DATA(overlap_min_z) = nmax( val1 = box-z1 val2 = b2-z1 ).
    DATA(overlap_max_z) = nmin( val1 = box-z2 val2 = b2-z2 ).

    IF overlap_max_x < overlap_min_x OR overlap_max_y < overlap_min_y OR overlap_max_z < overlap_min_z.
      CONTINUE.
    ENDIF.


    DATA(b3) = VALUE box_t(
        x1 = overlap_min_x
        x2 = overlap_max_x
        y1 = overlap_min_y
        y2 = overlap_max_y
        z1 = overlap_min_z
        z2 = overlap_max_z
    ).

    DATA(b3_volume) = CONV int8( 0 ).
    PERFORM get_volume USING b3 CHANGING b3_volume.

    DATA(b3_overlap) = CONV int8( 0 ).
    DATA(b3_boxes_start) = box_idx + CONV int8( 1 ).
    PERFORM get_overlap USING b3 boxes b3_boxes_start CHANGING b3_overlap.

    overlap = overlap + b3_volume - b3_overlap.

  ENDLOOP.

ENDFORM.