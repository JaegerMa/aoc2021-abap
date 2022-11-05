TYPES:
  BEGIN OF point_t,
    x TYPE i,
    y TYPE i,
    z TYPE i,
  END OF point_t,
  points_t    TYPE STANDARD TABLE OF point_t WITH DEFAULT KEY,
  point_set_t TYPE SORTED TABLE OF point_t WITH UNIQUE KEY x y z.

TYPES:
  BEGIN OF beacon_match_t,
    base_beacon    TYPE point_t,
    matched_beacon TYPE point_t,
  END OF beacon_match_t,
  beacon_matches_t TYPE STANDARD TABLE OF beacon_match_t WITH DEFAULT KEY.

CLASS scanner DEFINITION.

  PUBLIC SECTION.

    DATA center TYPE point_t.
    DATA local_beacons TYPE points_t.
    DATA rotation TYPE i.

    METHODS:
      constructor
        IMPORTING center        TYPE point_t OPTIONAL
                  local_beacons TYPE points_t OPTIONAL
                  rotation      TYPE i DEFAULT 0,

      get_global_beacons
        RETURNING VALUE(ret) TYPE points_t,
      rotate
        RETURNING VALUE(ret) TYPE REF TO scanner,
      set_base
        IMPORTING new_base   TYPE point_t
        RETURNING VALUE(ret) TYPE REF TO scanner,
      move_base
        IMPORTING p          TYPE point_t
        RETURNING VALUE(ret) TYPE REF TO scanner,
      translate_point
        IMPORTING point      TYPE point_t
        RETURNING VALUE(ret) TYPE point_t.

ENDCLASS.

TYPES scanners_t TYPE STANDARD TABLE OF REF TO scanner WITH DEFAULT KEY.


START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/19` CHANGING input.

  DATA raw_scanners TYPE scanners_t.
  DATA resolved_scanners TYPE scanners_t.
  PERFORM parse_input USING input CHANGING raw_scanners.
  PERFORM resolve_scanners USING raw_scanners CHANGING resolved_scanners.

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

  DATA unique_beacons TYPE point_set_t.
  LOOP AT resolved_scanners INTO DATA(scanner).

    LOOP AT scanner->get_global_beacons( ) INTO DATA(beacon).
      INSERT beacon INTO TABLE unique_beacons.
    ENDLOOP.

  ENDLOOP.


  DATA(answer) = lines( unique_beacons ).
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.
FORM part2.

  DATA(max_distance) = -1.

  LOOP AT resolved_scanners INTO DATA(a).
    LOOP AT resolved_scanners INTO DATA(b).

      DATA(distance) = abs( a->center-x - b->center-x ) + abs( a->center-y - b->center-y ) + abs( a->center-z - b->center-z ).
      max_distance = nmax( val1 = max_distance val2 = distance ).

    ENDLOOP.
  ENDLOOP.


  DATA(answer) = max_distance.
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.

FORM parse_input USING input TYPE string
              CHANGING scanners TYPE scanners_t.

  SPLIT input AT |\n\n| INTO TABLE DATA(blocks).

  LOOP AT blocks INTO DATA(block).

    DATA(local_beacons) = VALUE points_t( ).

    SPLIT block AT |\n| INTO TABLE DATA(beacon_strs).
    LOOP AT beacon_strs INTO DATA(beacon_str) FROM 2.

      SPLIT beacon_str AT ',' INTO DATA(x_str) DATA(y_str) DATA(z_str).
      DATA(beacon) = VALUE point_t( x = CONV i( x_str )
                                    y = CONV i( y_str )
                                    z = CONV i( z_str ) ).
      INSERT beacon INTO TABLE local_beacons.

    ENDLOOP.

    DATA(scanner) = NEW scanner( local_beacons = local_beacons ).
    INSERT scanner INTO TABLE scanners.

  ENDLOOP.

ENDFORM.

FORM resolve_scanners USING scanners TYPE scanners_t
                   CHANGING resolved TYPE scanners_t.


  DATA(core_scanner) = scanners[ 1 ].
  resolved = VALUE #( ( core_scanner ) ).

  DATA(remaining) = scanners.
  DELETE remaining INDEX 1.

  LOOP AT resolved INTO DATA(current).

    LOOP AT remaining INTO DATA(candidate).

      DATA resolved_scanner TYPE REF TO scanner.
      PERFORM resolve_scanner USING current candidate CHANGING resolved_scanner.
      IF resolved_scanner IS NOT BOUND.
        CONTINUE.
      ENDIF.

      INSERT resolved_scanner INTO TABLE resolved.
      DELETE remaining.

    ENDLOOP.

  ENDLOOP.

  IF lines( remaining ) > 0.
    MESSAGE 'Couldn''t resolve all scanners' TYPE 'E'.
  ENDIF.

ENDFORM.
FORM resolve_scanner USING base TYPE REF TO scanner
                           candidate TYPE REF TO scanner
                  CHANGING resolved TYPE REF TO scanner.


  DATA base_beacon_idx TYPE point_set_t.
  INSERT LINES OF base->get_global_beacons( ) INTO TABLE base_beacon_idx.

  DATA beacon_matches TYPE beacon_matches_t.
  PERFORM get_matching_beacon_candidates USING base candidate CHANGING beacon_matches.

  LOOP AT beacon_matches REFERENCE INTO DATA(match).

    DATA(current_candidate_variant) = candidate.

    DO 24 TIMES.

      DATA(translated_candidate_beacon) = current_candidate_variant->translate_point( match->matched_beacon ).
      DATA(new_base) = VALUE point_t( x = match->base_beacon-x - translated_candidate_beacon-x
                                      y = match->base_beacon-y - translated_candidate_beacon-y
                                      z = match->base_beacon-z - translated_candidate_beacon-z ).
      DATA(translated_candidate) = current_candidate_variant->set_base( new_base ).

      DATA(matching_beacon_count) = 0.
      LOOP AT translated_candidate->get_global_beacons( ) INTO DATA(check_beacon).

        IF line_exists( base_beacon_idx[ x = check_beacon-x y = check_beacon-y z = check_beacon-z ] ).

          matching_beacon_count = matching_beacon_count + 1.
          IF matching_beacon_count = 12.
            resolved = translated_candidate.
            RETURN.
          ENDIF.

        ENDIF.

      ENDLOOP.

      current_candidate_variant = current_candidate_variant->rotate( ).

    ENDDO.

  ENDLOOP.

  resolved = VALUE #( ).

ENDFORM.
FORM get_matching_beacon_candidates USING base TYPE REF TO scanner
                                          candidate TYPE REF TO scanner
                                 CHANGING matching_beacon_candidates TYPE beacon_matches_t.

  matching_beacon_candidates = VALUE #( ).

  DATA relatives_index TYPE SORTED TABLE OF i WITH UNIQUE KEY primary_key COMPONENTS table_line.

  DATA(base_beacons) = base->get_global_beacons( ).
  LOOP AT base_beacons INTO DATA(base_beacon) TO lines( base_beacons ) - 11.

    relatives_index = VALUE #( ).

    DATA(adjusted_base) = base->move_base( VALUE #( x = base_beacon-x * - 1 y = base_beacon-y * -1 z = base_beacon-z * -1 ) ).
    LOOP AT adjusted_base->get_global_beacons( ) INTO DATA(translated_base_beacon).

      INSERT abs( translated_base_beacon-x ) INTO TABLE relatives_index.
      INSERT abs( translated_base_beacon-y ) INTO TABLE relatives_index.
      INSERT abs( translated_base_beacon-z ) INTO TABLE relatives_index.

    ENDLOOP.


    DATA(candidate_beacons) = candidate->get_global_beacons( ).
    LOOP AT candidate_beacons INTO DATA(candidate_beacon) TO lines( candidate_beacons ) - 11.

      DATA(matching_relatives) = 0.

      DATA(adjusted_candidate) = candidate->move_base( VALUE #( x = candidate_beacon-x * -1 y = candidate_beacon-y * -1 z = candidate_beacon-z * -1 ) ).
      LOOP AT adjusted_candidate->get_global_beacons( ) INTO DATA(translated_candidate_beacon).

        IF line_exists( relatives_index[ table_line = abs( translated_candidate_beacon-x ) ] ).
          matching_relatives = matching_relatives + 1.
        ENDIF.
        IF line_exists( relatives_index[ table_line = abs( translated_candidate_beacon-y ) ] ).
          matching_relatives = matching_relatives + 1.
        ENDIF.
        IF line_exists( relatives_index[ table_line = abs( translated_candidate_beacon-z ) ] ).
          matching_relatives = matching_relatives + 1.
        ENDIF.

        IF matching_relatives >= 36.
          INSERT VALUE #( base_beacon = base_beacon matched_beacon = candidate_beacon ) INTO TABLE matching_beacon_candidates.
          EXIT.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDLOOP.

ENDFORM.


CLASS scanner IMPLEMENTATION.

  METHOD constructor.

    me->center = center.
    me->rotation = rotation.
    me->local_beacons = local_beacons.

  ENDMETHOD.

  METHOD get_global_beacons.

    ret = VALUE #( FOR local_beacon IN me->local_beacons ( me->translate_point( local_beacon ) ) ).

  ENDMETHOD.

  METHOD rotate.

    ret = NEW scanner( center = me->center
                       rotation = me->rotation + 1
                       local_beacons = me->local_beacons ).

  ENDMETHOD.

  METHOD set_base.

    ret = NEW scanner( center = new_base
                       rotation = me->rotation
                       local_beacons = me->local_beacons ).

  ENDMETHOD.

  METHOD move_base.

    DATA(new_base) = VALUE point_t( x = me->center-x + p-x
                                    y = me->center-y + p-y
                                    z = me->center-z + p-z ).
    ret = me->set_base( new_base ).

  ENDMETHOD.

  METHOD translate_point.

    DATA new_x TYPE i.
    DATA new_y TYPE i.
    DATA new_z TYPE i.

    CASE me->rotation MOD 24.

      WHEN 0.
        new_x = point-x.
        new_y = point-y.
        new_z = point-z.
      WHEN 1.
        new_x = point-x * -1.
        new_y = point-z * -1.
        new_z = point-y * -1.
      WHEN 2.
        new_x = point-x * -1.
        new_y = point-y.
        new_z = point-z * -1.
      WHEN 3.
        new_x = point-x * -1.
        new_y = point-z.
        new_z = point-y.
      WHEN 4.
        new_x = point-y * -1.
        new_y = point-x * -1.
        new_z = point-z * -1.
      WHEN 5.
        new_x = point-y * -1.
        new_y = point-z * -1.
        new_z = point-x.
      WHEN 6.
        new_x = point-y * -1.
        new_y = point-x.
        new_z = point-z.
      WHEN 7.
        new_x = point-y * -1.
        new_y = point-z.
        new_z = point-x * -1.
      WHEN 8.
        new_x = point-z * -1.
        new_y = point-x * -1.
        new_z = point-y.
      WHEN 9.
        new_x = point-z * -1.
        new_y = point-y * -1.
        new_z = point-x * -1.
      WHEN 10.
        new_x = point-z * -1.
        new_y = point-x.
        new_z = point-y * -1.
      WHEN 11.
        new_x = point-z * -1.
        new_y = point-y.
        new_z = point-x.
      WHEN 12.
        new_x = point-x.
        new_y = point-y * -1.
        new_z = point-z * -1.
      WHEN 13.
        new_x = point-x.
        new_y = point-z * -1.
        new_z = point-y.
      WHEN 14.
        new_x = point-x * -1.
        new_y = point-y * -1.
        new_z = point-z.
      WHEN 15.
        new_x = point-x.
        new_y = point-z.
        new_z = point-y * -1.
      WHEN 16.
        new_x = point-y.
        new_y = point-x * -1.
        new_z = point-z.
      WHEN 17.
        new_x = point-y.
        new_y = point-z * -1.
        new_z = point-x * -1.
      WHEN 18.
        new_x = point-y.
        new_y = point-x.
        new_z = point-z * -1.
      WHEN 19.
        new_x = point-y.
        new_y = point-z.
        new_z = point-x.
      WHEN 20.
        new_x = point-z.
        new_y = point-x * -1.
        new_z = point-y * -1.
      WHEN 21.
        new_x = point-z.
        new_y = point-y * -1.
        new_z = point-x.
      WHEN 22.
        new_x = point-z.
        new_y = point-x.
        new_z = point-y.
      WHEN 23.
        new_x = point-z.
        new_y = point-y.
        new_z = point-x * -1.

    ENDCASE.

    ret = VALUE #( x = me->center-x + new_x
                   y = me->center-y + new_y
                   z = me->center-z + new_z ).

  ENDMETHOD.

ENDCLASS.
