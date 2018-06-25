create or replace package as_barcode
is
  function barcode( p_val varchar2, p_type varchar2, p_parm varchar2 := null )
  return raw;
--
  procedure download_barcode( p_val varchar2, p_type varchar2, p_parm varchar2 := null );
--
  function datauri_barcode( p_val varchar2, p_type varchar2, p_parm varchar2 := null )
  return varchar2;
--
  function barcode_blob( p_val varchar2, p_type varchar2, p_parm varchar2 := null )
  return blob;
end;
/
