create or replace package as_barcode
is
  function barcode( p_val varchar2 character set any_cs
                  , p_type varchar2
                  , p_parm varchar2 := null
                  )
  return raw;
--
  function datauri_barcode( p_val varchar2 character set any_cs
                          , p_type varchar2
                          , p_parm varchar2 := null
                          )
  return clob;
--
  function barcode_svg( p_val varchar2 character set any_cs
                      , p_type varchar2
                      , p_parm varchar2 := null
                      )
  return clob;
--
  function datauri_barcode_svg( p_val varchar2 character set any_cs
                              , p_type varchar2
                              , p_parm varchar2 := null
                              )
  return clob;
--  
end;
/
