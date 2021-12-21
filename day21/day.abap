CLASS map DEFINITION.

  PUBLIC SECTION.

    TYPES:
      value_t TYPE int8,
      BEGIN OF entry_t,
        key   TYPE i,
        value TYPE value_t,
      END OF entry_t,
      entries_t TYPE HASHED TABLE OF entry_t WITH UNIQUE KEY primary_key COMPONENTS key.

    DATA entries TYPE entries_t.

    METHODS:
      get
        IMPORTING key        TYPE i
        RETURNING VALUE(ret) TYPE REF TO entry_t,
      get_value
        IMPORTING key        TYPE i
        RETURNING VALUE(ret) TYPE value_t.

ENDCLASS.


START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/21` CHANGING input.


  SPLIT input AT |\n| INTO TABLE DATA(lines).

  DATA(player1_line) = lines[ 1 ].
  DATA(player2_line) = lines[ 2 ].
  DATA(player1_start) = CONV i( player1_line+27 ).
  DATA(player2_start) = CONV i( player2_line+27 ).

  "1-based index
  "The key is the result of 3 rolls of a 3-sided dice.
  "The value is the number of times of all 27 dice-roll
  "  possibilities in which the key is the outcome.
  DATA(dice_odds_lookup) = VALUE int4_table( ( 0 ) ( 0 ) ( 1 ) ( 3 ) ( 6 ) ( 7 ) ( 6 ) ( 3 ) ( 1 ) ).


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

  "Using a 0-based game-field
  DATA(player1_pos) = player1_start - 1.
  DATA(player2_pos) = player2_start - 1.
  DATA(player1_score) = 0.
  DATA(player2_score) = 0.

  DATA(dice) = 0.
  DO.
    "dice contains the value before the roll.
    "Movement is (dice + 1) + (dice + 2) + (dice + 3)
    "which is simplified to dice * 3 + 6
    player1_pos = ( player1_pos + dice * 3 + 6 ) MOD 10.
    player1_score = player1_score + ( player1_pos + 1 ).
    dice = dice + 3.

    IF player1_score >= 1000.
      EXIT.
    ENDIF.


    player2_pos = ( player2_pos + dice * 3 + 6 ) MOD 10.
    player2_score = player2_score + ( player2_pos + 1 ).
    dice = dice + 3.

    IF player2_score >= 1000.
      EXIT.
    ENDIF.

  ENDDO.


  DATA(min_score) = nmin( val1 = player1_score val2 = player2_score ).

  DATA(answer) = min_score * dice.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.

FORM part2.

  DATA(player1_pos) = player1_start - 1.
  DATA(player2_pos) = player2_start - 1.

  DATA(win_map) = NEW map( ).
  PERFORM next_move USING 0 player1_pos 0 1 1 win_map.
  PERFORM next_move USING 0 player2_pos 0 1 2 win_map.


  DATA(player1_wins) = conv int8( 0 ).
  DATA(player2_wins) = conv int8( 0 ).
  DATA(player1_looses) = conv int8( 1 ).
  DATA(player2_looses) = conv int8( 1 ).

  DATA(max_move_id) = 0.
  LOOP AT win_map->entries REFERENCE INTO DATA(win_entry).
    max_move_id = nmax( val1 = max_move_id val2 = win_entry->key ).
  ENDLOOP.

  DO max_move_id TIMES.

    DATA(move_id) = sy-index.
    DATA(wins) = win_map->get_value( move_id ).
    IF move_id MOD 2 = 1.
      player1_wins = player1_wins + ( wins * player2_looses ).
      player1_looses = ( player1_looses * 27 ) - wins.
    ELSE.
      player2_wins = player2_wins + ( wins * player1_looses ).
      player2_looses = ( player2_looses * 27 ) - wins.
    ENDIF.

  ENDDO.


  DATA(max_wins) = nmax( val1 = player1_wins val2 = player2_wins ).

  DATA(answer) = max_wins.
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.

FORM next_move USING move_id TYPE i
                     position TYPE i
                     points TYPE i
                     active_player_universe_count TYPE int8
                     player_id TYPE i
                     win_map TYPE REF TO map.

  DO 7 TIMES.

    "Possible dice values in 3 rolls are in range [3, 9].
    DATA(dice_value) = sy-index + 2.
    DATA(dice_odds) = dice_odds_lookup[ dice_value ].

    DATA(next_position) = ( position + dice_value ) MOD 10.
    DATA(next_points) = points + next_position + 1.
    DATA(next_universe_count) = active_player_universe_count * CONV int8( dice_odds ).

    IF next_points >= 21.
      DATA(global_move_id) = move_id * 2 + player_id.
      DATA(win_entry) = win_map->get( global_move_id ).
      win_entry->value = win_entry->value + next_universe_count.
    ELSE.
      DATA(next_move_id) = move_id + 1.
      PERFORM next_move USING next_move_id next_position next_points next_universe_count player_id win_map.
    ENDIF.

  ENDDO.

ENDFORM.



CLASS map IMPLEMENTATION.

  METHOD get.

    DATA(entry) = REF #( me->entries[ key = key ] OPTIONAL ).
    IF entry IS NOT BOUND.
      INSERT VALUE #( key = key ) INTO TABLE me->entries REFERENCE INTO entry.
    ENDIF.

    ret = entry.

  ENDMETHOD.
  METHOD get_value.

    DATA(entry) = me->get( key ).
    ret = entry->value.

  ENDMETHOD.

ENDCLASS.
