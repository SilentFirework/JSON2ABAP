*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations - Fireworks
DEFINE eat_char.
  IF offset < length AND mv_json_in+offset(1) EQ &1.
    offset = offset + 1.
  ELSE.
    RETURN.
  ENDIF.
END-OF-DEFINITION.
DEFINE eat_name.
  IF mv_json_in+offset(1) EQ `"`.
    mark   = offset + 1.
    offset = mark.
    FIND FIRST OCCURRENCE OF `"` IN SECTION OFFSET offset OF mv_json_in MATCH OFFSET offset.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.
    match = offset - mark.
    &1 = mv_json_in+mark(match).
    offset = offset + 1.
    IF strlen( &1 ) > 30.
      &1 = &1+0(30).
    ENDIF.
  ELSE.
    RETURN.
  ENDIF.
END-OF-DEFINITION.

DEFINE eat_string.
  IF mv_json_in+offset(1) EQ `"`.
    mark   = offset + 1.
    offset = mark.
    DO.
      FIND FIRST OCCURRENCE OF `"` IN SECTION OFFSET offset OF mv_json_in MATCH OFFSET pos.
      IF sy-subrc IS NOT INITIAL.
        RETURN.
      ENDIF.
        offset = pos.
        pos = pos - 1.
        " if escaped search further
        WHILE pos GE 0 AND mv_json_in+pos(1) EQ `\`.
          pos = pos - 1.
        ENDWHILE.
      match = ( offset - pos ) MOD 2.
      IF match NE 0.
        EXIT.
      ENDIF.
      offset = offset + 1.
    ENDDO.
    match = offset - mark.
    &1 = mv_json_in+mark(match).
    " unescaped singe characters, e.g \\, \", \/ etc,
    " BUT ONLY if someone really need the data
    offset = offset + 1.
  ELSE.
    RETURN.
  ENDIF.
END-OF-DEFINITION.
DEFINE while_offset_cs.

  WHILE offset < length.
    FIND FIRST OCCURRENCE OF mv_json_in+offset(1) IN &1.
    IF sy-subrc IS NOT INITIAL.
      EXIT.
    ENDIF.
    offset = offset + 1.
  ENDWHILE.

END-OF-DEFINITION.
DEFINE eat_number.
  mark   = offset.
  while_offset_cs `0123456789+-eE.`.                        "#EC NOTEXT
  match = offset - mark.
  &1 = mv_json_in+mark(match).
END-OF-DEFINITION.

DEFINE eat_bool.
  mark   = offset.
  while_offset_cs `aeflnrstu`.                              "#EC NOTEXT
  match = offset - mark.
  IF mv_json_in+mark(match) EQ `true`.                      "#EC NOTEXT
    &1 = abap_true.
  ELSEIF mv_json_in+mark(match) EQ `false`.                 "#EC NOTEXT
      &1 = abap_false.
  ELSEIF mv_json_in+mark(match) EQ `null`.                  "#EC NOTEXT
    CLEAR &1.
  ENDIF.
END-OF-DEFINITION.
DEFINE skip_to_next_character.
  WHILE offset < length AND
        mv_json_in+offset(1) NA '",:{}[]tfn0123456789.+-eE'.
    offset = offset + 1.
  ENDWHILE.
end-of-definition.
