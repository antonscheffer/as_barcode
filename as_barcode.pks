create or replace package as_barcode
is
  function barcode( p_val varchar2 character set any_cs
                  , p_type varchar2
                  , p_parm varchar2 := null
                  )
  return raw;
--
  procedure download_barcode( p_val varchar2 character set any_cs
                            , p_type varchar2
                            , p_parm varchar2 := null
                            );
--
  function datauri_barcode( p_val varchar2 character set any_cs
                          , p_type varchar2
                          , p_parm varchar2 := null
                          , p_format varchar2 := null
                          )
  return clob;
--
  function barcode_svg( p_val       varchar2 character set any_cs
                      , p_type      varchar2
                      , p_parm      varchar2 := null
                      , p_logo      blob := null
                      , p_logo_href varchar2 := null
                      )
  return clob;
--
  function datauri_barcode_svg( p_val       varchar2 character set any_cs
                              , p_type      varchar2
                              , p_parm      varchar2 := null
                              , p_logo      blob := null
                              , p_logo_href varchar2 := null
                              )
  return clob;
--
  function barcode_blob( p_val       varchar2 character set any_cs
                       , p_type      varchar2
                       , p_parm      varchar2 := null
                       , p_format    varchar2 := 'BMP'
                       , p_logo      blob     := null
                       , p_logo_href varchar2 := null
                       )
  return blob;
--
end as_barcode;
/
