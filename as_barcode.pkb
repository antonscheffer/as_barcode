create or replace package body as_barcode
is
/**********************************************
**
** Author: Anton Scheffer
**
** Changelog:
**   Date: 2016-12-14
**     beta release
**   Date: 2017-02-11
**     version 0.20
**   Date: 2017-08-15
**     fixed bug in used png format
**   Date: 2022-11-04
**     added svg output
**   Date: 2022-12-14
**     started with datamatrix
******************************************************************************
******************************************************************************
Copyright (C) 2016-2022 by Anton Scheffer
 
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
 
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
 
******************************************************************************
******************************************** */
  type tp_bits is table of pls_integer index by pls_integer;
  type tp_matrix is table of tp_bits index by pls_integer;
  type tp_bar_row is table of number;
  type tp_mapping is table of tp_bar_row index by pls_integer;
  --
  function xjv
    ( p_json varchar2 character set any_cs
    , p_path varchar2
    , p_unescape varchar2 := 'Y'
    )
  return varchar2 character set p_json%charset
  is
    c_double_quote  constant varchar2(1) character set p_json%charset := '"';
    c_single_quote  constant varchar2(1) character set p_json%charset := '''';
    c_back_slash    constant varchar2(1) character set p_json%charset := '\';
    c_space         constant varchar2(1) character set p_json%charset := ' ';
    c_colon         constant varchar2(1) character set p_json%charset := ':';
    c_comma         constant varchar2(1) character set p_json%charset := ',';
    c_end_brace     constant varchar2(1) character set p_json%charset := '}';
    c_start_brace   constant varchar2(1) character set p_json%charset := '{';
    c_end_bracket   constant varchar2(1) character set p_json%charset := ']';
    c_start_bracket constant varchar2(1) character set p_json%charset := '[';
    c_ht            constant varchar2(1) character set p_json%charset := chr(9);
    c_lf            constant varchar2(1) character set p_json%charset := chr(10);
    c_cr            constant varchar2(1) character set p_json%charset := chr(13);
    c_ws            constant varchar2(4) character set p_json%charset := c_space || c_ht || c_cr || c_lf;
--
    g_idx number;
    g_end number;
--
    l_nchar boolean := isnchar( c_space );
    l_pos number;
    l_ind number;
    l_start number;
    l_rv_end number;
    l_rv_start number;
    l_path varchar2(32767);
    l_name varchar2(32767);
    l_tmp_name varchar2(32767);
    l_rv varchar2(32767) character set p_json%charset;
    l_chr varchar2(10) character set p_json%charset;
--
    procedure skip_whitespace
    is
    begin
      while substr( p_json, g_idx, 1 ) in ( c_space, c_lf, c_cr, c_ht )
      loop
        g_idx:= g_idx+ 1;
      end loop;
      if g_idx > g_end
      then
        raise_application_error( -20001, 'Unexpected end of JSON' );
      end if;
    end;
--
    procedure skip_value;
    procedure skip_array;
--
    procedure skip_object
    is
    begin
      if substr( p_json, g_idx, 1 ) = c_start_brace
      then
        g_idx := g_idx + 1;
        loop
          skip_whitespace;
          exit when substr( p_json, g_idx, 1 ) = c_end_brace; -- empty object or object with "trailing comma"
          skip_value; -- skip name
          skip_whitespace;
          if substr( p_json, g_idx, 1 ) != c_colon
          then
            raise_application_error( -20002, 'No valid JSON, expected a colon at position ' || g_idx );
          end if;
          g_idx := g_idx + 1; -- skip colon
          skip_value; -- skip value
          skip_whitespace;
          case substr( p_json, g_idx, 1 )
            when c_comma then g_idx := g_idx + 1;
            when c_end_brace then exit;
            else raise_application_error( -20003, 'No valid JSON, expected a comma or end brace at position ' || g_idx );
          end case;
        end loop;
        g_idx := g_idx + 1;
      end if;
    end;
--
    procedure skip_array
    is
    begin
      if substr( p_json, g_idx, 1 ) = c_start_bracket
      then
        g_idx := g_idx + 1;
        loop
          skip_whitespace;
          exit when substr( p_json, g_idx, 1 ) = c_end_bracket; -- empty array or array with "trailing comma"
          skip_value;
          skip_whitespace;
          case substr( p_json, g_idx, 1 )
            when c_comma then g_idx := g_idx + 1;
            when c_end_bracket then exit;
            else raise_application_error( -20004, 'No valid JSON, expected a comma or end bracket at position ' || g_idx );
          end case;
        end loop;
        g_idx := g_idx + 1;
      end if;
    end;
--
    procedure skip_value
    is
    begin
      skip_whitespace;
      case substr( p_json, g_idx, 1 )
        when c_double_quote
        then
          loop
            g_idx := instr( p_json, c_double_quote, g_idx + 1 );
            exit when substr( p_json, g_idx - 1, 1 ) != c_back_slash
                   or g_idx = 0
                   or (   substr( p_json, g_idx - 2, 2 ) = c_back_slash || c_back_slash
                      and substr( p_json, g_idx - 3, 1 ) != c_back_slash
                      ); -- doesn't handle cases of values ending with multiple (escaped) \
          end loop;
          if g_idx = 0
          then
            raise_application_error( -20005, 'No valid JSON, no end string found' );
          end if;
          g_idx := g_idx + 1;
        when c_single_quote
        then
          g_idx := instr( p_json, c_single_quote, g_idx ) + 1;
          if g_idx = 1
          then
            raise_application_error( -20006, 'No valid JSON, no end string found' );
          end if;
        when c_start_brace
        then
          skip_object;
        when c_start_bracket
        then
          skip_array;
        else -- should be a JSON-number, TRUE, FALSE or NULL, but we don't check for it
          g_idx := least( coalesce( nullif( instr( p_json, c_space, g_idx ), 0 ), g_end + 1 )
                        , coalesce( nullif( instr( p_json, c_comma, g_idx ), 0 ), g_end + 1 )
                        , coalesce( nullif( instr( p_json, c_end_brace, g_idx ), 0 ), g_end + 1 )
                        , coalesce( nullif( instr( p_json, c_end_bracket, g_idx ), 0 ), g_end  + 1)
                        , coalesce( nullif( instr( p_json, c_colon, g_idx ), 0 ), g_end + 1 )
                        );
          if g_idx = g_end + 1
          then
            raise_application_error( -20007, 'No valid JSON, no end string found' );
          end if;
      end case;
    end;
  begin
    if p_json is null
    then
      return null;
    end if;
    l_path := ltrim( p_path, c_ws );
    if l_path is null
    then
      return null;
    end if;
    g_idx := 1;
    g_end := length( p_json );
    for i in 1 .. 20 -- max 20 levels deep in p_path
    loop
      l_path := ltrim( l_path, c_ws );
      l_pos := least( nvl( nullif( instr( l_path, '.' ), 0 ), 32768 )
                    , nvl( nullif( instr( l_path, c_start_bracket ), 0 ), 32768 )
                    , nvl( nullif( instr( l_path, c_end_bracket ), 0 ), 32768 )
                    );
      if l_pos = 32768
      then
        l_name := l_path;
        l_path := null;
      elsif substr( l_path, l_pos, 1 ) = '.'
      then
        l_name := substr( l_path, 1, l_pos - 1 );
        l_path := substr( l_path, l_pos + 1 );
      elsif substr( l_path, l_pos, 1 ) = c_start_bracket and l_pos > 1
      then
        l_name := substr( l_path, 1, l_pos - 1 );
        l_path := substr( l_path, l_pos );
      elsif substr( l_path, l_pos, 1 ) = c_start_bracket and l_pos = 1
      then
        l_pos := instr( l_path, c_end_bracket );
        if l_pos = 0
        then
          raise_application_error( -20008, 'No valid path, end bracket expected' );
        end if;
        l_name := substr( l_path, 1, l_pos );
        if substr( l_path, l_pos + 1, 1 ) = '.'
        then
          l_path := substr( l_path, l_pos + 2 );
        else
          l_path := substr( l_path, l_pos + 1 );
        end if;
      end if;
      l_name := rtrim( l_name, c_ws );
--
      skip_whitespace;
      if substr( p_json, g_idx, 1 ) = c_start_brace and substr( l_name, 1, 1 ) != c_start_bracket
      then -- search for a name inside JSON object
           -- json unescape name?
        loop
          g_idx := g_idx + 1; -- skip start brace or comma
          skip_whitespace;
          if substr( p_json, g_idx, 1 ) = c_end_brace
          then
            return null;
          end if;
          l_start := g_idx;
          skip_value;  -- skip a name
          l_tmp_name := substr( p_json, l_start, g_idx - l_start ); -- look back to get the name skipped
           -- json unescape name?
          skip_whitespace;
          if substr( p_json, g_idx, 1 ) != c_colon
          then
            raise_application_error( -20002, 'No valid JSON, expected a colon at position ' || g_idx );
          end if;
          g_idx := g_idx + 1;  -- skip colon
          skip_whitespace;
          l_rv_start := g_idx;
          skip_value;
          if l_tmp_name in ( c_double_quote || l_name || c_double_quote
                           , c_single_quote || l_name || c_single_quote
                           , l_name
                           )
          then
            l_rv_end := g_idx;
            exit;
          else
            skip_whitespace;
            if substr( p_json, g_idx, 1 ) = c_comma
            then
              null; -- OK, keep on searching for name
            else
              return null; -- searched name not found
            end if;
          end if;
        end loop;
      elsif substr( p_json, g_idx, 1 ) = c_start_bracket and substr( l_name, 1, 1 ) = c_start_bracket
      then
        begin
          l_ind := to_number( rtrim( ltrim( l_name, c_start_bracket ), c_end_bracket ) );
        exception
          when value_error
          then
            raise_application_error( -20009, 'No valid path, array index number expected' );
        end;
        for i in 0 .. l_ind loop
          g_idx := g_idx + 1; -- skip start bracket or comma
          skip_whitespace;
          if substr( p_json, g_idx, 1 ) = c_end_bracket
          then
            return null;
          end if;
          l_rv_start := g_idx;
          skip_value;
          if i = l_ind
          then
            l_rv_end := g_idx;
            exit;
          else
            skip_whitespace;
            if substr( p_json, g_idx, 1 ) = c_comma
            then
              null; -- OK
            else
              return null;
            end if;
          end if;
        end loop;
      else
        return null;
      end if;
      exit when l_path is null;
      g_idx := l_rv_start;
      g_end := l_rv_end - 1;
    end loop;
    if (  (   substr( p_json, l_rv_start, 1 ) = c_double_quote
          and substr( p_json, l_rv_end - 1, 1 ) = c_double_quote
          )
       or (   substr( p_json, l_rv_start, 1 ) = c_single_quote
          and substr( p_json, l_rv_end - 1, 1 ) = c_single_quote
          )
       )
    then
      l_rv_start := l_rv_start + 1;
      l_rv_end := l_rv_end - 1;
    end if;
    l_pos := instr( p_json, c_back_slash, l_rv_start );
    if l_pos = 0 or l_pos >= l_rv_end or nvl( substr( upper( p_unescape ), 1, 1 ), 'Y' ) = 'N'
    then -- no JSON unescaping needed
      return substr( p_json, l_rv_start, l_rv_end - l_rv_start );
    end if;
    l_start := l_rv_start;
    loop
      l_chr := substr( p_json, l_pos + 1, 1 );
      if l_chr in ( '"', '\', '/' )
      then
        l_rv := l_rv || ( substr( p_json, l_start, l_pos - l_start ) || l_chr );
      elsif l_chr in ( 'b', 'f', 'n', 'r', 't' )
      then
        l_chr := translate( l_chr
                          , 'btnfr'
                          , chr(8) || chr(9) || chr(10) || chr(12) || chr(13)
                          );
        l_rv := l_rv || ( substr( p_json, l_start, l_pos - l_start ) || l_chr );
      elsif l_chr = 'u'
      then -- unicode character
        if l_nchar
        then
          l_chr := utl_i18n.raw_to_nchar( hextoraw( substr( p_json, l_pos + 2, 4 ) ), 'AL16UTF16' );
        else
          l_chr := utl_i18n.raw_to_char( hextoraw( substr( p_json, l_pos + 2, 4 ) ), 'AL16UTF16' );
        end if;
        l_rv := l_rv || ( substr( p_json, l_start, l_pos - l_start ) || l_chr );
        l_pos := l_pos + 4;
      else
        raise_application_error( -20011, 'No valid JSON, unexpected back slash  at position ' || l_pos );
      end if;
      l_start := l_pos + 2;
      l_pos := instr( p_json, c_back_slash, l_start );
      if l_pos = 0 or l_pos >= l_rv_end
      then
        l_rv := l_rv || substr( p_json, l_start, l_rv_end - l_start );
        exit;
      end if;
    end loop;
    return l_rv;
  end;
  --
  function jv( p_json varchar2 character set any_cs
             , p_path varchar2
             , p_unescape varchar2 := 'Y'
             )
  return varchar2 character set p_json%charset
  is
  begin
    return xjv( p_json, p_path, p_unescape );
  exception when others then
    return null;
  end;
  --
  function check_int( p_parm varchar2, p_name varchar2, p_max pls_integer := null )
  return pls_integer
  is
    l_int pls_integer;
  begin
    l_int := trunc( to_number( xjv( p_parm, p_name ) ) );
    if l_int not between 0 and nvl( p_max, l_int )
    then
      l_int := null;
    end if;
    return l_int;
  exception when others then return null;
  end;
 
  function check_pos( p_parm varchar2, p_name varchar2 )
  return boolean
  is
  begin
    return lower( jv( p_parm, p_name ) ) in ( 'true', 'y', 'yes' );
  end;
  --
  function check_neg( p_parm varchar2, p_name varchar2 )
  return boolean
  is
  begin
    return lower( jv( p_parm, p_name ) ) in ( 'false', 'n', 'no' );
  end;
  --
  function generate_png( p_dat blob, p_width pls_integer, p_height pls_integer, p_parm varchar2 )
  return raw
  is
    t_ihdr  raw(25);
    t_plte  raw(32);
    t_idat  raw(32767);
    t_dark  raw(3) := '000000';
    t_light raw(3) := 'FFFFFF';
    --
    function crc32( p_src raw )
    return raw
    is
    begin
      return utl_raw.reverse( utl_raw.substr( utl_raw.substr( utl_compress.lz_compress( p_src ), -8 ), 1, 4 ) );
    end;
    --
    function method0_compress( p_val blob )
    return raw
    is
      t_tmp raw(32767);
      function adler32( p_val blob )
      return varchar2
      is
        s1 pls_integer := 1;
        s2 pls_integer := 0;
        t_val varchar2(32766);
        t_pos number := 1;
        t_len number := dbms_lob.getlength( p_val );
      begin
        loop
          exit when t_pos > t_len;
          t_val := rawtohex( dbms_lob.substr( p_val, 16383, t_pos ) );
          for i in 1 .. length( t_val ) / 2
          loop
            begin
              s1 := s1 + to_number( substr( t_val, i * 2 - 1, 2 ), 'XX' );
            exception
              when others then
                s1 := mod( s1, 65521 ) + to_number( substr( t_val, i * 2 - 1, 2 ), 'XX' );
            end;
            begin
              s2 := s2 + s1;
            exception
              when others then
                s2 := mod( s2, 65521 ) + s1;
            end;
          end loop;
          t_pos := t_pos + 16383;
        end loop;
        s1 := mod( s1, 65521 );
        s2 := mod( s2, 65521 );
        return to_char( s2, 'fm0XXX' ) || to_char( s1, 'fm0XXX' );
      end;
    begin
      t_tmp := utl_compress.lz_compress( p_val );
      return utl_raw.concat( '789C'
                           , utl_raw.substr( t_tmp, 11, utl_raw.length( t_tmp ) - 18 )
                           , adler32( p_val )
                           );
    end;
    --
  begin
    t_ihdr := utl_raw.concat( '49484452' -- IHDR
                            , to_char( p_width, 'fm0XXXXXXX' )
                            , to_char( p_height, 'fm0XXXXXXX' )
                            , '0803000000'  -- Bit depth 8
                                            -- Colour type 3 palet
                                            -- Compression method 0
                                            -- Filter method 0
                                            -- Interlace method 0
                            );
    t_plte := utl_raw.concat( '504C5445' -- PLTE
                            , t_dark
                            , t_light
                            );
    t_idat := utl_raw.concat( '49444154' -- IDAT
                            , method0_compress( p_dat )
                            );
    return utl_raw.concat( '89504E470D0A1A0A' -- signature
                         , '0000000D'         -- length IHDR
                         , t_ihdr
                         , crc32( t_ihdr )
                         , '00000006'         -- length PLTE
                         , t_plte
                         , crc32( t_plte )
                         , to_char( utl_raw.length( t_idat ) - 4, 'fm0XXXXXXX' )
                         , t_idat
                         , crc32( t_idat )
                         , '0000000049454E44AE426082' -- IEND
                         );
  end generate_png;
  --
  function bitxor( x number, y number )
  return number
  is
  begin
    return x + y - 2 * bitand( x, y );
  end;
  --
  procedure append_bits( p_bits in out tp_bits, p_val number, p_cnt number )
  is
  begin
    for j in reverse 0 .. p_cnt - 1
    loop
      p_bits( p_bits.count ) := sign( bitand( p_val, power( 2, j ) ) );
    end loop;
  end;
  --
  function bitstoword( p_bits tp_bits, p_sz pls_integer )
  return tp_bits
  is
    l_val pls_integer;
    l_rv tp_bits;
    l_first pls_integer := p_bits.first;
  begin
    for i in l_first ..  p_bits.count / p_sz - l_first - 1
    loop
      l_val := 0;
      for j in 0 .. p_sz - 1
      loop
        l_val := l_val * 2 + p_bits( l_first + ( i - l_first ) * p_sz + j );
      end loop;
      l_rv( i - l_first ) := l_val;
    end loop;
    return l_rv;
  end;
  --
  function reed_solomon
    ( p_data tp_bits
    , p_primitive pls_integer := 285
    , p_size pls_integer := 256
    , p_degree pls_integer := 16
    , p_b pls_integer := 1
    )
  return tp_bits
  is
    type tp_ecc is table of pls_integer index by pls_integer;
    t_exp tp_ecc;
    t_log tp_ecc;
    t_g tp_ecc;
    t_ecc tp_ecc;
    t_x pls_integer;
    t_rv tp_bits;
--
  begin
    t_x := 1;
    for i in 0 .. p_size - 1
    loop
      t_exp( i ) := t_x;
      t_x := t_x * 2;
      if t_x >= p_size
      then
        t_x := bitand( p_size - 1, bitxor( p_primitive, t_x ) );
      end if;
    end loop;
    for i in 0 .. p_size - 2
    loop
      t_log( t_exp( i ) ) := i;
    end loop;
--
    t_g(0) := 1;
    for i in 1 .. p_degree
    loop
      t_x := i - 1 + p_b;
      t_g(i) := t_exp( mod( t_log( t_g( i-1 ) ) + t_x, p_size - 1 ) );
      for j in reverse 1 .. i - 1
      loop
        t_g(j) := bitxor( t_exp( mod( t_log(t_g(j-1)) + t_x, p_size - 1 ) )
                        , t_g(j)
                        );
      end loop;
-- t_log( 1 ) is altijd 0
 --     t_g(0) := bitxor( 0, t_exp( t_log( t_g( 0 ) ) + t_log( 1 ) ) );
    end loop;
--
    t_x := p_data.first;
    for i in t_x .. p_data.last
    loop
      t_ecc( i - t_x ) := p_data( i );
    end loop;
    for i in t_ecc.count .. t_ecc.count + p_degree - 1
    loop
      t_ecc( i ) := 0;
    end loop;
    --
    while t_ecc.count >= t_g.count
    loop
      t_x := t_ecc( t_ecc.first );
      if t_x > 0 then
        for i in 0 .. t_g.count - 1
        loop
            t_ecc( t_ecc.first + i ) := bitxor( t_ecc( t_ecc.first + i )
                                              , t_exp( mod( t_log( t_g( i ) ) + t_log( t_x ), p_size - 1 ) )
                                              );
        end loop;
      end if;
      t_ecc.delete( t_ecc.first );
    end loop;
    --
    t_x := t_ecc.first;
    for i in t_ecc.first .. t_ecc.last
    loop
      t_rv( i - t_x ) := t_ecc( i );
    end loop;
    return t_rv;
  end;
  --
  function upc_checksum( p_val in varchar2 )
  return varchar2
  is
    l_tmp pls_integer := 0;
  begin
    for i in 1 .. length( p_val )
    loop
      l_tmp := l_tmp + to_number( substr( p_val, - i, 1 ) )
                       * ( 1 + 2 * mod( i, 2 ) );
    end loop;
    return to_char( ceil( l_tmp / 10 ) * 10 - l_tmp, 'fm0' );
  end;
  --
  procedure add_quiet( p_matrix in out nocopy tp_matrix, p_parm varchar2, p_quiet pls_integer )
  is
    l_height pls_integer := p_matrix.count;
    l_width  pls_integer := p_matrix( p_matrix.first ).count;
    l_quiet pls_integer;
  begin
    l_quiet := coalesce( check_int( p_parm, 'quiet' ), p_quiet, 0 );
    if l_quiet = 0
    then
      return;
    end if;
    for i in reverse 0 .. l_height - 1 loop
      for j in reverse 0 .. l_width - 1 loop
        p_matrix( i + l_quiet )( j + l_quiet ) := p_matrix(i)(j);
      end loop;
      for j in 0 .. l_quiet - 1 loop
        p_matrix( i + l_quiet )(j) := 0;
        p_matrix( i + l_quiet )( j + l_width + l_quiet ) := 0;
      end loop;
    end loop;
    for j in 0 .. l_width + 2 * l_quiet - 1 loop
      p_matrix(0)(j) := 0;
    end loop;
    for i in 0 .. l_quiet - 1 loop
      p_matrix(i) := p_matrix(0);
      p_matrix( i + l_quiet + l_height ) := p_matrix(0);
    end loop;
  end add_quiet;
    --
  procedure gen_qrcode_matrix( p_val varchar2 character set any_cs
                             , p_parm varchar2
                             , p_matrix out tp_matrix
                             )
  is
    l_version pls_integer;
    l_eclevel pls_integer;
    l_stream tp_bits;
    l_tmp    raw(32767);
    l_sz  pls_integer;
    l_len pls_integer;
    type tp_config is table of pls_integer;
    type tp_ecc_config is table of tp_config;
    type tp_qr_config is table of tp_ecc_config;
    l_qr_config tp_qr_config;
    --
    function get_formatinfo( p_eclevel in pls_integer, p_mask pls_integer )
    return pls_integer
    is
      type tp_format is table of tp_config;
      l_format tp_format;
    begin
      l_format := tp_format( tp_config( 30660, 29427, 32170, 30877, 26159, 25368, 27713, 26998 )
                           , tp_config( 21522, 20773, 24188, 23371, 17913, 16590, 20375, 19104 )
                           , tp_config( 13663, 12392, 16177, 14854, 9396, 8579, 11994, 11245 )
                           , tp_config( 5769, 5054, 7399, 6608, 1890, 597, 3340, 2107 )
                           );
      return l_format( p_eclevel )( p_mask + 1 );
    end;
    --
    procedure add_patterns( p_version pls_integer
                          , p_matrix in out nocopy tp_matrix
                          )
    is
      l_width pls_integer := 4 * p_version + 17;
      type tp_inf is table of pls_integer;
      type tp_pos is table of pls_integer;
      type tp_align is table of tp_pos;
      l_align tp_align;
      l_info tp_inf;
      l_version_info pls_integer;
      l_cnt pls_integer;
      l_bit pls_integer;
      --
      procedure add_finder( p_x pls_integer, p_y pls_integer, p_w pls_integer )
      is
        l_sx pls_integer := case p_w when 2 then l_width -  8 else 7 end;
        l_sy pls_integer := case p_w when 3 then l_width -  8 else 7 end;
        l_dx pls_integer := case p_w when 2 then 1 else -1 end;
        l_dy pls_integer := case p_w when 3 then 1 else -1 end;
      begin
        for i in -3 .. 3 loop
          for j in -3 .. 3 loop
            p_matrix( p_x + i )( p_y + j ) := 1;
          end loop;
        end loop;
        for i in -2 .. 2 loop
          p_matrix( p_x + i )( p_y - 2 ) := 0;
          p_matrix( p_x + i )( p_y + 2 ) := 0;
          p_matrix( p_x - 2 )( p_y + i ) := 0;
          p_matrix( p_x + 2 )( p_y - i ) := 0;
        end loop;
        for i in 0 .. 7 loop
          p_matrix( p_x + ( i - 4 ) * l_dx )( p_y - 4 * l_dy ) := 0;
          if p_w != 3
          then  -- reserved for format information
            p_matrix( p_x + ( i - 4 ) * l_dx )( p_y + 5 ) := 1;
          end if;
          p_matrix( p_x - 4 * l_dx )( p_y + ( i - 4 ) * l_dy ) := 0;
          if p_w != 2
          then  -- reserved for format information
            p_matrix( p_x + 5 )( p_y + ( i - 4 ) * l_dy ) := 1;
          end if;
        end loop;
      end;
      --
      procedure add_aligment( p_x pls_integer, p_y pls_integer )
      is
      begin
        for i in -2 .. 2 loop
          for j in -2 .. 2 loop
            p_matrix( p_x + i )( p_y + j ) := 1;
          end loop;
        end loop;
        for i in -1 .. 1 loop
          p_matrix( p_x + i )( p_y - 1 ) := 0;
          p_matrix( p_x + i )( p_y + 1 ) := 0;
        end loop;
        p_matrix( p_x + 1 )( p_y ) := 0;
        p_matrix( p_x - 1 )( p_y ) := 0;
      end;
    begin
      for r in 0 .. l_width - 1
      loop
        for c in 0 .. l_width - 1
        loop
          p_matrix( r )( c ) := 3; -- init everything to dark
        end loop;
      end loop;
      --
      add_finder( 3, 3, 1 );
      add_finder( l_width - 4, 3, 2 );
      add_finder( 3, l_width - 4, 3 );
      p_matrix( 8 )( 8 ) := 1; -- reserved for format information
      --
      for i in 8 .. l_width - 9 loop
        p_matrix( i )( 6 ) := 1 - mod( i, 2 ); -- timing
        p_matrix( 6 )( i ) := 1 - mod( i, 2 ); -- timing
      end loop;
      --
      if p_version > 1
      then
        add_aligment( l_width - 7, l_width - 7 );
        if p_version > 6
        then
          l_align := tp_align( tp_pos( 6, 22, 38 ) -- 7
                             , tp_pos( 6, 24, 42 ) -- 8
                             , tp_pos( 6, 26, 46 ) -- 9
                             , tp_pos( 6, 28, 50 ) -- 10
                             , tp_pos( 6, 30, 54 ) -- 11
                             , tp_pos( 6, 32, 58 ) -- 12
                             , tp_pos( 6, 34, 62 ) -- 13
                             , tp_pos( 6, 26, 46, 66 ) -- 14
                             , tp_pos( 6, 26, 48, 70 ) -- 15
                             , tp_pos( 6, 26, 50, 74 ) -- 16
                             , tp_pos( 6, 30, 54, 78 ) -- 17
                             , tp_pos( 6, 30, 56, 82 ) -- 18
                             , tp_pos( 6, 30, 58, 86 ) -- 19
                             , tp_pos( 6, 34, 62, 90 ) -- 20
                             , tp_pos( 6, 28, 50, 72,  94 ) -- 21
                             , tp_pos( 6, 26, 50, 74,  98 ) -- 22
                             , tp_pos( 6, 30, 54, 78, 102 ) -- 23
                             , tp_pos( 6, 28, 54, 80, 106 ) -- 24
                             , tp_pos( 6, 32, 58, 84, 110 ) -- 25
                             , tp_pos( 6, 30, 58, 86, 114 ) -- 26
                             , tp_pos( 6, 34, 62, 90, 118 ) -- 27
                             , tp_pos( 6, 26, 50, 74,  98, 122 ) -- 28
                             , tp_pos( 6, 30, 54, 78, 102, 126 ) -- 29
                             , tp_pos( 6, 26, 52, 78, 104, 130 ) -- 30
                             , tp_pos( 6, 30, 56, 82, 108, 134 ) -- 31
                             , tp_pos( 6, 34, 60, 86, 112, 138 ) -- 32
                             , tp_pos( 6, 30, 58, 86, 114, 142 ) -- 33
                             , tp_pos( 6, 34, 62, 90, 118, 146 ) -- 34
                             , tp_pos( 6, 30, 54, 78, 102, 126, 150 ) -- 35
                             , tp_pos( 6, 24, 50, 76, 102, 128, 154 ) -- 36
                             , tp_pos( 6, 28, 54, 80, 106, 132, 158 ) -- 37
                             , tp_pos( 6, 32, 58, 84, 110, 136, 162 ) -- 38
                             , tp_pos( 6, 26, 54, 82, 110, 138, 166 ) -- 39
                             , tp_pos( 6, 30, 58, 86, 114, 142, 170 ) -- 40
                             );
          l_cnt := l_align( l_version - 6 ).count;
          for i in 1 .. l_cnt loop
            for j in 1 .. l_cnt loop
              if i between 2 and l_cnt - 1 or j between 2 and l_cnt - 1
              then
                add_aligment( l_align( l_version - 6 )( i )
                            , l_align( l_version - 6 )( j )
                            );
              end if;
            end loop;
          end loop;
          --
          l_info := tp_inf
            ( 31892, 34236, 39577, 42195, 48118, 51042, 55367, 58893, 63784
            , 68472, 70749, 76311, 79154, 84390, 87683, 92361, 96236, 102084
            , 102881, 110507, 110734, 117786, 119615, 126325, 127568, 133589
            , 136944, 141498, 145311, 150283, 152622, 158308, 161089, 167017
            );
          l_version_info := l_info( l_version - 6 );
          for i in 0 .. 5
          loop
            for j in 0 .. 2
            loop
              l_bit := sign( bitand( l_version_info, power( 2, i * 3 + j ) ) );
              p_matrix( l_width - 11 + j )( i ) := l_bit; -- lower left
              p_matrix( i )( l_width - 11 + j ) := l_bit; -- upper right
            end loop;
          end loop;
        end if;
      end if;
    end add_patterns;
    --
    procedure add_stream( p_width pls_integer
                        , p_stream tp_bits
                        , p_matrix in out nocopy tp_matrix
                        )
    is
      l_x pls_integer;
      l_y pls_integer;
      l_direction pls_integer := -1;
      procedure next_pos
      is
      begin
        if l_x is null
        then
          l_x := p_width - 1;
          l_y := p_width - 1;
        else
          if (  l_x > 5 and mod( l_x, 2 ) = 0
             or l_x < 6 and mod( l_x, 2 ) = 1
             )
          then
            l_x := l_x - 1;
          else
            l_x := l_x + 1;
            l_y := l_y + l_direction;
          end if;
          if l_y < 0
          then
            l_x := l_x - case when l_x = 8 then 3 else 2 end; -- skip vertical timing column
            l_y := 0;
            l_direction := 1;
          elsif l_y >= p_width
          then
            l_x := l_x - 2;
            l_y := p_width - 1;
            l_direction := - 1;
          end if;
          if l_y = 6 or l_x = 6 or p_matrix( l_x )( l_y ) != 3
          then
            next_pos;
          end if;
        end if;
      end;
    begin
      for i in 0 .. p_stream.count - 1
      loop
        next_pos;
        p_matrix( l_x )( l_y ) := 128 + p_stream( i );
      end loop;
      -- remainder bits
      for i in 0 .. 1 loop
        for j in 9 .. p_width - 8 loop
          if p_matrix( i )( j ) between 2 and 127
          then
            p_matrix( i )( j ) := 128;
          end if;
        end loop;
      end loop;
    end add_stream;
    --
    function get_qr_config
    return tp_qr_config
    is
    begin
      return tp_qr_config( tp_ecc_config( tp_config( 19,7,41,25,17,16,1,1,19 ) -- 1
                                        , tp_config( 16,10,34,20,14,13,1,1,16 )
                                        , tp_config( 13,13,27,16,11,10,1,1,13 )
                                        , tp_config( 9,17,17,10,7,6,1,1,9 )
                                        )
                         , tp_ecc_config( tp_config( 34,10,77,47,32,31,1,1,34 ) -- 2
                                        , tp_config( 28,16,63,38,26,25,1,1,28 )
                                        , tp_config( 22,22,48,29,20,19,1,1,22 )
                                        , tp_config( 16,28,34,20,14,13,1,1,16 )
                                        )
                         , tp_ecc_config( tp_config( 55,15,127,77,53,52,1,1,55 ) -- 3
                                        , tp_config( 44,26,101,61,42,41,1,1,44 )
                                        , tp_config( 34,36,77,47,32,31,1,2,17 )
                                        , tp_config( 26,44,58,35,24,23,1,2,13 )
                                        )
                         , tp_ecc_config( tp_config( 80,20,187,114,78,77,1,1,80 ) -- 4
                                        , tp_config( 64,36,149,90,62,61,1,2,32 )
                                        , tp_config( 48,52,111,67,46,45,1,2,24 )
                                        , tp_config( 36,64,82,50,34,33,1,4,9 )
                                        )
                         , tp_ecc_config( tp_config( 108,26,255,154,106,105,1,1,108 ) -- 5
                                        , tp_config( 86,48,202,122,84,83,1,2,43 )
                                        , tp_config( 62,72,144,87,60,59,2,2,15,2,16 )
                                        , tp_config( 46,88,106,64,44,43,2,2,11,2,12 )
                                        )
                         , tp_ecc_config( tp_config( 136,36,322,195,134,133,1,2,68 ) -- 6
                                        , tp_config( 108,64,255,154,106,105,1,4,27 )
                                        , tp_config( 76,96,178,108,74,73,1,4,19 )
                                        , tp_config( 60,112,139,84,58,57,1,4,15 )
                                        )
                         , tp_ecc_config( tp_config( 156,40,370,224,154,153,1,2,78 ) -- 7
                                        , tp_config( 124,72,293,178,122,121,1,4,31 )
                                        , tp_config( 88,108,207,125,86,85,2,2,14,4,15 )
                                        , tp_config( 66,130,154,93,64,63,2,4,13,1,14 )
                                        )
                         , tp_ecc_config( tp_config( 194,48,461,279,192,191,1,2,97 ) -- 8
                                        , tp_config( 154,88,365,221,152,151,2,2,38,2,39 )
                                        , tp_config( 110,132,259,157,108,107,2,4,18,2,19 )
                                        , tp_config( 86,156,202,122,84,83,2,4,14,2,15 )
                                        )
                         , tp_ecc_config( tp_config( 232,60,552,335,230,229,1,2,116 ) -- 9
                                        , tp_config( 182,110,432,262,180,179,2,3,36,2,37 )
                                        , tp_config( 132,160,312,189,130,129,2,4,16,4,17 )
                                        , tp_config( 100,192,235,143,98,97,2,4,12,4,13 )
                                        )
                         , tp_ecc_config( tp_config( 274,72,652,395,271,270,2,2,68,2,69 ) -- 10
                                        , tp_config( 216,130,513,311,213,212,2,4,43,1,44 )
                                        , tp_config( 154,192,364,221,151,150,2,6,19,2,20 )
                                        , tp_config( 122,224,288,174,119,118,2,6,15,2,16 )
                                        )
                         , tp_ecc_config( tp_config( 324,80,772,468,321,320,1,4,81 ) -- 11
                                        , tp_config( 254,150,604,366,251,250,2,1,50,4,51 )
                                        , tp_config( 180,224,427,259,177,176,2,4,22,4,23 )
                                        , tp_config( 140,264,331,200,137,136,2,3,12,8,13 )
                                        )
                         , tp_ecc_config( tp_config( 370,96,883,535,367,366,2,2,92,2,93 ) -- 12
                                        , tp_config( 290,176,691,419,287,286,2,6,36,2,37 )
                                        , tp_config( 206,260,489,296,203,202,2,4,20,6,21 )
                                        , tp_config( 158,308,374,227,155,154,2,7,14,4,15 )
                                        )
                         , tp_ecc_config( tp_config( 428,104,1022,619,425,424,1,4,107 ) -- 13
                                        , tp_config( 334,198,796,483,331,330,2,8,37,1,38 )
                                        , tp_config( 244,288,580,352,241,240,2,8,20,4,21 )
                                        , tp_config( 180,352,427,259,177,176,2,12,11,4,12 )
                                        )
                         , tp_ecc_config( tp_config( 461,120,1101,667,458,457,2,3,115,1,116 ) -- 14
                                        , tp_config( 365,216,871,528,362,361,2,4,40,5,41 )
                                        , tp_config( 261,320,621,376,258,257,2,11,16,5,17 )
                                        , tp_config( 197,384,468,283,194,193,2,11,12,5,13 )
                                        )
                         , tp_ecc_config( tp_config( 523,132,1250,758,520,519,2,5,87,1,88 ) -- 15
                                        , tp_config( 415,240,991,600,412,411,2,5,41,5,42 )
                                        , tp_config( 295,360,703,426,292,291,2,5,24,7,25 )
                                        , tp_config( 223,432,530,321,220,219,2,11,12,7,13 )
                                        )
                         , tp_ecc_config( tp_config( 589,144,1408,854,586,585,2,5,98,1,99 ) -- 16
                                        , tp_config( 453,280,1082,656,450,449,2,7,45,3,46 )
                                        , tp_config( 325,408,775,470,322,321,2,15,19,2,20 )
                                        , tp_config( 253,480,602,365,250,249,2,3,15,13,16 )
                                        )
                         , tp_ecc_config( tp_config( 647,168,1548,938,644,643,2,1,107,5,108 ) -- 17
                                        , tp_config( 507,308,1212,734,504,503,2,10,46,1,47 )
                                        , tp_config( 367,448,876,531,364,363,2,1,22,15,23 )
                                        , tp_config( 283,532,674,408,280,279,2,2,14,17,15 )
                                        )
                         , tp_ecc_config( tp_config( 721,180,1725,1046,718,717,2,5,120,1,121 ) -- 18
                                        , tp_config( 563,338,1346,816,560,559,2,9,43,4,44 )
                                        , tp_config( 397,504,948,574,394,393,2,17,22,1,23 )
                                        , tp_config( 313,588,746,452,310,309,2,2,14,19,15 )
                                        )
                         , tp_ecc_config( tp_config( 795,196,1903,1153,792,791,2,3,113,4,114 ) -- 19
                                        , tp_config( 627,364,1500,909,624,623,2,3,44,11,45 )
                                        , tp_config( 445,546,1063,644,442,441,2,17,21,4,22 )
                                        , tp_config( 341,650,813,493,338,337,2,9,13,16,14 )
                                        )
                         , tp_ecc_config( tp_config( 861,224,2061,1249,858,857,2,3,107,5,108 ) -- 20
                                        , tp_config( 669,416,1600,970,666,665,2,3,41,13,42 )
                                        , tp_config( 485,600,1159,702,482,481,2,15,24,5,25 )
                                        , tp_config( 385,700,919,557,382,381,2,15,15,10,16 )
                                        )
                         , tp_ecc_config( tp_config( 932,224,2232,1352,929,928,2,4,116,4,117 ) -- 21
                                        , tp_config( 714,442,1708,1035,711,710,1,17,42 )
                                        , tp_config( 512,644,1224,742,509,508,2,17,22,6,23 )
                                        , tp_config( 406,750,969,587,403,402,2,19,16,6,17 )
                                        )
                         , tp_ecc_config( tp_config( 1006,252,2409,1460,1003,1002,2,2,111,7,112 ) -- 22
                                        , tp_config( 782,476,1872,1134,779,778,1,17,46 )
                                        , tp_config( 568,690,1358,823,565,564,2,7,24,16,25 )
                                        , tp_config( 442,816,1056,640,439,438,1,34,13 )
                                        )
                         , tp_ecc_config( tp_config( 1094,270,2620,1588,1091,1090,2,4,121,5,122 ) -- 23
                                        , tp_config( 860,504,2059,1248,857,856,2,4,47,14,48 )
                                        , tp_config( 614,750,1468,890,611,610,2,11,24,14,25 )
                                        , tp_config( 464,900,1108,672,461,460,2,16,15,14,16 )
                                        )
                         , tp_ecc_config( tp_config( 1174,300,2812,1704,1171,1170,2,6,117,4,118 ) -- 24
                                        , tp_config( 914,560,2188,1326,911,910,2,6,45,14,46 )
                                        , tp_config( 664,810,1588,963,661,660,2,11,24,16,25 )
                                        , tp_config( 514,960,1228,744,511,510,2,30,16,2,17 )
                                        )
                         , tp_ecc_config( tp_config( 1276,312,3057,1853,1273,1272,2,8,106,4,107 ) -- 25
                                        , tp_config( 1000,588,2395,1451,997,996,2,8,47,13,48 )
                                        , tp_config( 718,870,1718,1041,715,714,2,7,24,22,25 )
                                        , tp_config( 538,1050,1286,779,535,534,2,22,15,13,16 )
                                        )
                         , tp_ecc_config( tp_config( 1370,336,3283,1990,1367,1366,2,10,114,2,115 ) -- 26
                                        , tp_config( 1062,644,2544,1542,1059,1058,2,19,46,4,47 )
                                        , tp_config( 754,952,1804,1094,751,750,2,28,22,6,23 )
                                        , tp_config( 596,1110,1425,864,593,592,2,33,16,4,17 )
                                        )
                         , tp_ecc_config( tp_config( 1468,360,3517,2132,1465,1464,2,8,122,4,123 ) -- 27
                                        , tp_config( 1128,700,2701,1637,1125,1124,2,22,45,3,46 )
                                        , tp_config( 808,1020,1933,1172,805,804,2,8,23,26,24 )
                                        , tp_config( 628,1200,1501,910,625,624,2,12,15,28,16 )
                                        )
                         , tp_ecc_config( tp_config( 1531,390,3669,2223,1528,1527,2,3,117,10,118 ) -- 28
                                        , tp_config( 1193,728,2857,1732,1190,1189,2,3,45,23,46 )
                                        , tp_config( 871,1050,2085,1263,868,867,2,4,24,31,25 )
                                        , tp_config( 661,1260,1581,958,658,657,2,11,15,31,16 )
                                        )
                         , tp_ecc_config( tp_config( 1631,420,3909,2369,1628,1627,2,7,116,7,117 ) -- 29
                                        , tp_config( 1267,784,3035,1839,1264,1263,2,21,45,7,46 )
                                        , tp_config( 911,1140,2181,1322,908,907,2,1,23,37,24 )
                                        , tp_config( 701,1350,1677,1016,698,697,2,19,15,26,16 )
                                        )
                         , tp_ecc_config( tp_config( 1735,450,4158,2520,1732,1731,2,5,115,10,116 ) -- 30
                                        , tp_config( 1373,812,3289,1994,1370,1369,2,19,47,10,48 )
                                        , tp_config( 985,1200,2358,1429,982,981,2,15,24,25,25 )
                                        , tp_config( 745,1440,1782,1080,742,741,2,23,15,25,16 )
                                        )
                         , tp_ecc_config( tp_config( 1843,480,4417,2677,1840,1839,2,13,115,3,116 ) -- 31
                                        , tp_config( 1455,868,3486,2113,1452,1451,2,2,46,29,47 )
                                        , tp_config( 1033,1290,2473,1499,1030,1029,2,42,24,1,25 )
                                        , tp_config( 793,1530,1897,1150,790,789,2,23,15,28,16 )
                                        )
                         , tp_ecc_config( tp_config( 1955,510,4686,2840,1952,1951,1,17,115 ) -- 32
                                        , tp_config( 1541,924,3693,2238,1538,1537,2,10,46,23,47 )
                                        , tp_config( 1115,1350,2670,1618,1112,1111,2,10,24,35,25 )
                                        , tp_config( 845,1620,2022,1226,842,841,2,19,15,35,16 )
                                        )
                         , tp_ecc_config( tp_config( 2071,540,4965,3009,2068,2067,2,17,115,1,116 ) -- 33
                                        , tp_config( 1631,980,3909,2369,1628,1627,2,14,46,21,47 )
                                        , tp_config( 1171,1440,2805,1700,1168,1167,2,29,24,19,25 )
                                        , tp_config( 901,1710,2157,1307,898,897,2,11,15,46,16 )
                                        )
                         , tp_ecc_config( tp_config( 2191,570,5253,3183,2188,2187,2,13,115,6,116 ) -- 34
                                        , tp_config( 1725,1036,4134,2506,1722,1721,2,14,46,23,47 )
                                        , tp_config( 1231,1530,2949,1787,1228,1227,2,44,24,7,25 )
                                        , tp_config( 961,1800,2301,1394,958,957,2,59,16,1,17 )
                                        )
                         , tp_ecc_config( tp_config( 2306,570,5529,3351,2303,2302,2,12,121,7,122 ) -- 35
                                        , tp_config( 1812,1064,4343,2632,1809,1808,2,12,47,26,48 )
                                        , tp_config( 1286,1590,3081,1867,1283,1282,2,39,24,14,25 )
                                        , tp_config( 986,1890,2361,1431,983,982,2,22,15,41,16 )
                                        )
                         , tp_ecc_config( tp_config( 2434,600,5836,3537,2431,2430,2,6,121,14,122 ) -- 36
                                        , tp_config( 1914,1120,4588,2780,1911,1910,2,6,47,34,48 )
                                        , tp_config( 1354,1680,3244,1966,1351,1350,2,46,24,10,25 )
                                        , tp_config( 1054,1980,2524,1530,1051,1050,2,2,15,64,16 )
                                        )
                         , tp_ecc_config( tp_config( 2566,630,6153,3729,2563,2562,2,17,122,4,123 ) -- 37
                                        , tp_config( 1992,1204,4775,2894,1989,1988,2,29,46,14,47 )
                                        , tp_config( 1426,1770,3417,2071,1423,1422,2,49,24,10,25 )
                                        , tp_config( 1096,2100,2625,1591,1093,1092,2,24,15,46,16 )
                                        )
                         , tp_ecc_config( tp_config( 2702,660,6479,3927,2699,2698,2,4,122,18,123 ) -- 38
                                        , tp_config( 2102,1260,5039,3054,2099,2098,2,13,46,32,47 )
                                        , tp_config( 1502,1860,3599,2181,1499,1498,2,48,24,14,25 )
                                        , tp_config( 1142,2220,2735,1658,1139,1138,2,42,15,32,16 )
                                        )
                         , tp_ecc_config( tp_config( 2812,720,6743,4087,2809,2808,2,20,117,4,118 ) -- 39
                                        , tp_config( 2216,1316,5313,3220,2213,2212,2,40,47,7,48 )
                                        , tp_config( 1582,1950,3791,2298,1579,1578,2,43,24,22,25 )
                                        , tp_config( 1222,2310,2927,1774,1219,1218,2,10,15,67,16 )
                                        )
                         , tp_ecc_config( tp_config( 2956,750,7089,4296,2953,2952,2,19,118,6,119 ) -- 40
                                        , tp_config( 2334,1372,5596,3391,2331,2330,2,18,47,31,48 )
                                        , tp_config( 1666,2040,3993,2420,1663,1662,2,34,24,34,25 )
                                        , tp_config( 1276,2430,3057,1852,1273,1272,2,20,15,61,16 )
                                        )
                         );
    end get_qr_config;
    --
    function get_version( p_len pls_integer
                        , p_eclevel pls_integer
                        , p_mode pls_integer
                        , p_parm varchar2
                        )
    return pls_integer
    is
      l_version pls_integer;
      l_tmp pls_integer;
    begin
      l_version := 1;
      while p_len > l_qr_config( l_version )( p_eclevel )( p_mode )
      loop
        l_version := l_version + 1;
      end loop;
      begin
        l_tmp := xjv( p_parm, 'version' );
        if l_tmp between l_version + 1 and 40
        then
          l_version := l_tmp;
        end if;
      exception when others then null;
      end;
      return l_version;
    end get_version;
    --
    procedure add_byte_data( p_val raw, p_version pls_integer, p_stream in out nocopy tp_bits )
    is
      l_len pls_integer := utl_raw.length( p_val );
    begin
      append_bits( p_stream, 4, 4 );  -- byte mode
      append_bits( p_stream, l_len, case when p_version <= 9 then 8 else 16 end );
      for i in 1 .. l_len
      loop
        append_bits( p_stream, to_number( utl_raw.substr( p_val, i, 1 ), 'xx' ), 8 );
      end loop;
    end add_byte_data;
    --
  begin
    l_eclevel := case upper( jv( p_parm, 'eclevel' ) )
                   when 'L' then 1
                   when 'M' then 2
                   when 'Q' then 3
                   when 'H' then 4
                   else 2
                 end;
    l_qr_config := get_qr_config;
    --
    if translate( p_val, '#0123456789', '#' ) is null
    then  -- numeric mode
      l_version := get_version( length( p_val ), l_eclevel, 3, p_parm );
      append_bits( l_stream, 1, 4 ); -- mode
      append_bits( l_stream, length( p_val )
                 , case
                     when l_version <= 9 then 10
                     when l_version <= 26 then 12
                     else 14
                   end
                 );
      for i in 1 .. trunc( length( p_val ) / 3 )
      loop
        append_bits( l_stream, substr( p_val, i * 3 - 2, 3 ), 10 );
      end loop;
      case mod( length( p_val ), 3 )
        when 1 then append_bits( l_stream, substr( p_val, -1 ), 4 );
        when 2 then append_bits( l_stream, substr( p_val, -2 ), 7 );
        else null;
      end case;
    elsif translate( p_val, '#0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:', '#' ) is null
    then -- alphanumeric mode
      l_version := get_version( length( p_val ), l_eclevel, 4, p_parm );
      append_bits( l_stream, 2, 4 ); -- mode
      append_bits( l_stream, length( p_val )
                 , case
                     when l_version <= 9 then 9
                     when l_version <= 26 then 11
                     else 13
                   end
                 );
      l_tmp := utl_raw.translate( utl_raw.cast_to_raw( p_val )
                                , utl_raw.concat( utl_raw.xrange( '30', '39' )
                                                , utl_raw.xrange( '41', '5A' )
                                                , '2024252A2B2D2E2F3A'
                                                )
                                , utl_raw.xrange( '00', '2C' )
                                );
      for i in 1 .. trunc( length( p_val ) / 2 )
      loop
        append_bits( l_stream
                   , to_number( utl_raw.substr( l_tmp, i * 2 - 1, 1 ), 'xx' ) * 45
                   + to_number( utl_raw.substr( l_tmp, i * 2, 1 ), 'xx' )
                   , 11
                   );
      end loop;
      if mod( length( p_val ), 2 ) = 1
      then
        append_bits( l_stream, to_number( utl_raw.substr( l_tmp, -1 ), 'xx' ), 6 );
      end if;
    elsif (   (   isnchar( p_val )
              and utl_i18n.raw_to_nchar( utl_i18n.string_to_raw( p_val, 'US7ASCII' ), 'US7ASCII' ) = p_val
          )
          or (   not isnchar( p_val )
             and utl_i18n.raw_to_char( utl_i18n.string_to_raw( p_val, 'US7ASCII' ), 'US7ASCII' ) = p_val
             )
          )
    then -- byte mode
      l_version := get_version( length( p_val ), l_eclevel, 5, p_parm );
      l_tmp := utl_i18n.string_to_raw( p_val, 'US7ASCII' );
      add_byte_data( l_tmp, l_version, l_stream );
    else -- ECI mode
      l_version := get_version( length( p_val ), l_eclevel, 6, p_parm );
      append_bits( l_stream, 7, 4 );  -- ECI mode
      append_bits( l_stream, 26, 8 ); -- ECI Assignment number 26 = UTF8
      if check_neg( p_parm, 'double_backslash' )
      then
        l_tmp := utl_i18n.string_to_raw( p_val, 'AL32UTF8' );
      else
        l_tmp := utl_i18n.string_to_raw( replace( p_val, '\', '\\' ), 'AL32UTF8' );
      end if;
      add_byte_data( l_tmp, l_version, l_stream );
    end if;
    -- terminator
    l_sz := l_qr_config( l_version )( l_eclevel )( 1 ) * 8;
    for i in 1 .. 4
    loop
      if l_stream.count < l_sz
      then
        append_bits( l_stream, 0, 1 );
      end if;
    end loop;
    -- 8-bit alignment
    if mod( l_stream.count, 8 ) > 0
    then
      append_bits( l_stream, 0, 8 - mod( l_stream.count, 8 ) );
    end if;
    -- padding
    l_len := l_stream.count;
    loop
      exit when l_len >= l_sz;
      append_bits( l_stream, 236, 8 );
      l_len := l_len + 8;
      exit when l_len >= l_sz;
      append_bits( l_stream, 17, 8 );
      l_len := l_len + 8;
    end loop;
    --
    declare
      l_data      tp_bits;
      l_ecc       tp_bits;
      l_blocks    pls_integer;
      l_block_idx pls_integer;
      l_ec_bytes  pls_integer;
      l_dw_bytes  pls_integer;
      l_offs      pls_integer;
      l_noffs     pls_integer;
      l_eoffs     pls_integer;
      l_block tp_bits;
      l_new   tp_bits;
    begin
      l_blocks := l_qr_config( l_version)( l_eclevel )( 8 );
      if l_qr_config( l_version)( l_eclevel )( 7 ) > 1
      then
        l_blocks := l_blocks + l_qr_config( l_version)( l_eclevel )( 10 );
      end if;
      l_ec_bytes := l_qr_config( l_version)( l_eclevel )( 2 ) / l_blocks;
      l_data := bitstoword( l_stream, 8 );
      l_offs := 0;
      l_noffs := 0;
      l_block_idx := 0;
      l_eoffs := l_qr_config( l_version)( l_eclevel )( 1 );
      for i in 1 .. l_qr_config( l_version)( l_eclevel )( 7 )
      loop
        l_dw_bytes := l_qr_config( l_version)( l_eclevel )( 7 + i * 2 );
        for j in 1 .. l_qr_config( l_version)( l_eclevel )( 6 + i * 2 )
        loop
          l_noffs := l_block_idx;
          for x in 0 .. l_dw_bytes - 1
          loop
            l_block( x ) := l_data( x + l_offs );
            l_new( l_noffs ) := l_block( x );
            if i > 1 and x >= l_qr_config( l_version)( l_eclevel )( 9 ) - 1
            then
              l_noffs := l_noffs + l_qr_config( l_version)( l_eclevel )( 10 );
            else
              l_noffs := l_noffs + l_blocks;
            end if;
          end loop;
          l_offs := l_offs + l_dw_bytes;
          l_ecc := reed_solomon( l_block, 285, 256, l_ec_bytes, 0 );
          for x in 0 .. l_ec_bytes - 1
          loop
            l_new( l_eoffs + l_block_idx + x * l_blocks ) := l_ecc( x );
          end loop;
          l_block.delete;
          l_ecc.delete;
          l_block_idx := l_block_idx + 1;
        end loop;
      end loop;
          l_stream.delete;
          for i in l_new.first .. l_new.last
          loop
            append_bits( l_stream, l_new( i ), 8 );
          end loop;
    end;
    --
    add_patterns( l_version, p_matrix );
    add_stream( 4 * l_version + 17, l_stream, p_matrix );
    --
    l_stream.delete;
    --
    declare
      l_width pls_integer := 4 * l_version + 17;
      l_mask pls_integer;
      l_hbit pls_integer;
      l_hcnt pls_integer;
      l_vbit pls_integer;
      l_vcnt pls_integer;
      masked tp_matrix;
      n1 pls_integer;
      n2 pls_integer;
      n3 pls_integer;
      n4 pls_integer;
      best number;
      score number;
    function mask_function( f pls_integer
                          , i pls_integer
                          , j pls_integer
                          )
    return pls_integer
    is
    begin
      return nvl( case
                    when f = 0 and mod( i+j, 2 ) = 0 then 1
                    when f = 1 and mod( i, 2 ) = 0 then 1
                    when f = 2 and mod( j, 3 ) = 0 then 1
                    when f = 3 and mod( i+j, 3 ) = 0 then 1
                    when f = 4 and mod(trunc(i/2)+trunc(j/3),2) = 0 then 1
                    when f = 5 and mod(i*j,2) + mod(i*j,3) = 0 then 1
                    when f = 6 and mod(mod(i*j,2) + mod(i*j,3), 2 ) = 0 then 1
                    when f = 7 and mod(mod(i*j,3) + mod(i+j,2), 2 ) = 0 then 1
                  end
                , 0
                );
    end;
    --
    procedure mask_matrix
      ( p_mat tp_matrix
      , p_masked in out nocopy tp_matrix
      , p_mask pls_integer
      )
    is
      t_info pls_integer;
    begin
      for y in 0 .. l_width - 1
      loop
        for x in 0 .. l_width - 1
        loop
          if p_mat( y )( x ) > 127
          then
            p_masked( y )( x ) := bitxor( p_mat( y )( x ) - 128, mask_function( p_mask, x, y ) );
          else
            p_masked( y )( x ) := p_mat( y )( x );
          end if;
        end loop;
      end loop;
      t_info := get_formatinfo( l_eclevel, p_mask );
      for i in 0 .. 5
      loop
        p_masked(i)(8) := sign( bitand( t_info, power( 2, 14 - i ) ) );
        p_masked(8)(i) := sign( bitand( t_info, power( 2, i ) ) );
      end loop;
      p_masked(7)(8) := sign( bitand( t_info, power( 2, 8 ) ) );
      p_masked(8)(8) := sign( bitand( t_info, power( 2, 7 ) ) );
      p_masked(8)(7) := sign( bitand( t_info, power( 2, 6 ) ) );
      for i in 0 .. 6
      loop
        p_masked(8)(l_width-1-i) := sign( bitand( t_info, power( 2, 14 - i ) ) );
        p_masked(l_width-1-i)(8) := sign( bitand( t_info, power( 2, i ) ) );
      end loop;
      p_masked(l_width - 8)(8) := sign( bitand( t_info, power( 2, 7 ) ) );
    end;
    --
    procedure score_rule1( p_cnt pls_integer )
    is
    begin
      if p_cnt >= 5
      then
        n1 := n1 + 3 + p_cnt - 5;
      end if;
    end;
    --
    procedure rule1( p_bit pls_integer
                   , p_prev in out pls_integer
                   , p_cnt in out pls_integer
                   )
    is
    begin
      if p_bit = p_prev
      then
        p_cnt := p_cnt + 1;
      else
        score_rule1( p_cnt );
        p_prev := p_bit;
        p_cnt := 1;
      end if;
    end;
    --
    procedure rule3( p_x pls_integer, p_y pls_integer, p_xy boolean )
    is
      function gfm( p pls_integer )
      return pls_integer
      is
      begin
        return sign( case when p_xy
                       then masked( p_y )( p_x + p )
                       else masked( p_y + p )( p_x )
                     end
                   );
      end;
    begin
      if (   case when p_xy then p_x else p_y end >= 6
         and gfm( - 6 ) = 1
         and gfm( - 5 ) = 0
         and gfm( - 4 ) = 1
         and gfm( - 3 ) = 1
         and gfm( - 2 ) = 1
         and gfm( - 1 ) = 0
         and gfm( 0 ) = 1
         and (  (   case when p_xy then p_x else p_y end >= 10
                and gfm( - 7 ) + gfm( - 8 ) + gfm( - 9 ) + gfm( - 10 ) = 0
                )
             or (   case when p_xy then p_x else p_y end <= l_width - 5
                and  gfm( 1 ) + gfm( 2 ) + gfm( 3 ) + gfm( 4 ) = 0
                )
             )
         )
      then
        n3 := n3 + 1;
      end if;
    end;
    --
    begin
      if xjv( p_parm, 'mask' ) between '0' and '7'
      then
        l_mask := xjv( p_parm, 'mask' );
      else
        best := 99999999;
        for m in 0 .. 7
        loop
          mask_matrix( p_matrix, masked, m );
          n1 := 0;
          n2 := 0;
          n3 := 0;
          n4 := 0;
          for y in 0 .. l_width - 1
          loop
            l_hbit := -1;
            l_hcnt := 0;
            l_vbit := -1;
            l_vcnt := 0;
            for x in 0 .. l_width - 1
            loop
              rule1( sign( masked(y)(x) ), l_hbit, l_hcnt );
              rule1( sign( masked(x)(y) ), l_vbit, l_vcnt );
              --
              if ( x > 0 and y > 0
                 and ( sign( masked(y)(x) ) + sign( masked(y)(x-1) )
                     + sign( masked(y-1)(x) ) + sign( masked(y-1)(x-1) )
                     ) in ( 0, 4 )
                 )
              then
                n2 := n2 + 1;
              end if;
              --
              rule3( x, y, true );
              rule3( x, y, false );
              --
              n4 := n4 + sign( masked(y)(x) );
            end loop;
            score_rule1( l_hcnt );
            score_rule1( l_vcnt );
          end loop;
          n4 := trunc( 10 * abs( n4 * 2 - l_width * l_width ) / ( l_width * l_width ) );
          score := n1 + n2 * 3 + n3 * 40 + n4 * 10;
          if score < best
          then
            l_mask := m;
            best := score;
          end if;
        end loop;
      end if;
      mask_matrix( p_matrix, p_matrix, l_mask );
    end;
    --
    add_quiet( p_matrix, p_parm, 4 );
    --
  end gen_qrcode_matrix;
  --
  procedure gen_aztec_matrix( p_val varchar2 character set any_cs
                            , p_parm varchar2
                            , p_matrix out tp_matrix
                            )
  is
    l_bits  tp_bits;
    l_sbits tp_bits;
    l_ecc_factor number;
    l_ecc_size pls_integer;
    l_total_size pls_integer;
    l_bil pls_integer;
    l_mode_msg tp_bits;
    l_layers   pls_integer;
    l_wordsize pls_integer;
    l_compact  boolean;
    --
    procedure get_bits( p_val raw, p_bits in out tp_bits )
    is
      l_idx pls_integer := 1;
      l_cur_mode pls_integer := 1;
      l_char pls_integer;
      l_mode pls_integer;
      l_len pls_integer;
      l_codes tp_bits;
      l_modes tp_bits;
      l_x raw(128);
      l_y pls_integer;
      l_bs_len pls_integer;
      l_geg varchar2(250) :=
        'AGJjZGVmZ2hpamtsbYEAAAAAAAAAAAAAAAAAb3BxcnOhhoeIiYqLjI2Oj5Cskq2U' ||
        'oqOkpaanqKmqq5WWl5iZmnQiIyQlJicoKSorLC0uLzAxMjM0NTY3ODk6O5t1nHZ3' ||
        'eEJDREVGR0hJSktMTU5PUFFSU1RVVldYWVpbnXmeens=';
      --
      procedure add_code( p_code pls_integer, p_mode pls_integer )
      is
      begin
        if p_mode != 5
        then
          p_bits( p_bits.count ) := sign( bitand( p_code, 16 ) );
        end if;
        p_bits( p_bits.count ) := sign( bitand( p_code, 8 ) );
        p_bits( p_bits.count ) := sign( bitand( p_code, 4 ) );
        p_bits( p_bits.count ) := sign( bitand( p_code, 2 ) );
        p_bits( p_bits.count ) := sign( bitand( p_code, 1 ) );
      end;
      --
      function get_char( p_offs pls_integer := 1 )
      return pls_integer
      is
      begin
        return to_number( utl_raw.substr( p_val, l_idx + p_offs, 1 ), 'XX' );
      exception
        when others then return null;
      end;
      --
      function get_mode( p_offs pls_integer := 1 )
      return pls_integer
      is
      begin
        return l_modes( get_char( p_offs ) );
      exception
        when others then return null;
      end;
    begin
      l_x := utl_encode.base64_decode( utl_raw.cast_to_raw( l_geg ) );
      for i in 0 .. 127
      loop
        l_codes( i + 128 ) := 0;
        l_modes( i + 128 ) := 0;
        l_y := to_number( utl_raw.substr( l_x, i + 1, 1 ), 'xx' );
        l_codes( i ) := mod( l_y, 32 );
        l_modes( i ) := trunc( l_y / 32 );
      end loop;
      --
      l_len := utl_raw.length( p_val );
      loop
        exit when l_idx > l_len;
        l_char := get_char( 0 );
        l_mode := l_modes( l_char );
        if (  l_mode = l_cur_mode
           or l_char = 32
           )
        then
          add_code( l_codes( l_char ), l_cur_mode );
        else
          if l_mode = 5
          then
            if l_char in ( 44, 46 ) and get_char = 32
            then
              add_code( 0, 1 ); -- Punct Shift, assumes l_cur_mode is never 4
              add_code( case when l_char = 44 then 4 else 3 end, 4 );
              l_idx := l_idx + 1;
            else
              if l_cur_mode = 3
              then
                add_code( 29, 3 );  -- Upper Latch
              end if;
              add_code( 30, 1 );    -- Digit Latch
              l_cur_mode := l_mode;
              add_code( l_codes( l_char ), l_mode );
            end if;
          elsif l_mode = 4
          then
            add_code( 0, l_cur_mode );  -- Punct Shift
            if l_char = 13 and get_char = 10
            then
              add_code( 2, 4 );
              l_idx := l_idx + 1;
            elsif l_char = 58 and get_char = 32
            then
              add_code( 5, 4 );
              l_idx := l_idx + 1;
            else
              add_code( l_codes( l_char ), 4 );
            end if;
          elsif l_mode = 3
          then
            if l_cur_mode = 5
            then
              add_code( 14, 5 );  -- Upper Latch
            end if;
            add_code( 29, 1 );  -- Mixed Latch
            l_cur_mode := 3;
            add_code( l_codes( l_char ), l_mode );
          elsif l_mode = 2
          then
            if l_cur_mode = 5
            then
              add_code( 14, 5 );  -- Upper Latch
            end if;
            add_code( 28, 1 );    -- Lower Latch
            l_cur_mode := 2;
            add_code( l_codes( l_char ), l_mode );
          elsif l_mode = 1
          then
            if l_cur_mode = 3
            then
              add_code( 29, 3 ); -- Upper Latch
              l_cur_mode := 1;
            elsif l_cur_mode = 2
            then
              case get_mode
                when 2
                then
                  add_code( 28, 2 ); -- Upper Switch
                when 5
                then
                  add_code( 30, 2 );  -- Digit Latch
                  l_cur_mode := 5;
                  add_code( 15, 5 );  -- Upper Switch
                else
                  add_code( 30, 2 );  -- Digit Latch
                  add_code( 14, 5 );  -- Upper Latch
                  l_cur_mode := 1;
                end case;
            else -- l_cur_mode = 5
              if get_mode = 1
              then
                add_code( 14, 5 );  -- Upper Latch
                l_cur_mode := 1;
              else
                add_code( 15, 5 );  -- Upper Switch
              end if;
            end if;
            add_code( l_codes( l_char ), 1 );
          else  -- l_mode = 0
            if l_cur_mode = 5
            then
              add_code( 14, 5 ); -- UL Upper Latch
              l_cur_mode := 1;
            end if;
            for i in 0 .. l_len
            loop
              exit when nvl( get_mode( i ), -1 ) != 0;
              l_bs_len := i + 1;
            end loop;
            add_code( 31, 1 ); -- BS Binary Shift
            if l_bs_len < 32
            then
              append_bits( p_bits, l_bs_len, 5 );
            else
              append_bits( p_bits, l_bs_len - 31, 11 );
            end if;
            for i in 0 .. l_bs_len - 1
            loop
              append_bits( p_bits, get_char( i ), 8 );
            end loop;
            l_idx := l_idx + l_bs_len - 1;
          end if;
        end if;
        l_idx := l_idx + 1;
      end loop;
    end;
    --
    procedure get_matrix( p_matrix in out tp_matrix )
    is
      l_tmp_matrix_size  pls_integer;
      l_tmp_center       pls_integer;
      l_matrix_size      pls_integer;
      l_center           pls_integer;
      l_offs             pls_integer;
      l_row_offs         pls_integer;
      l_row_size         pls_integer;
      type tp_array is table of pls_integer index by pls_integer;
      l_remap  tp_array;
      l_matrix tp_array;
      --
      procedure one_block( x pls_integer, y pls_integer, v pls_integer )
      is
      begin
        p_matrix( x )( y ) := v;
      end;
      --
      procedure init_matrix( sz pls_integer )
      is
      begin
        for i in 0 .. sz - 1
        loop
          for j in 0 .. sz - 1
          loop
            one_block( i, j, 0 );
          end loop;
        end loop;
      end;
      --
      procedure line( c pls_integer, p pls_integer )
      is
      begin
        for i in 0 .. 2 * p - 1
        loop
          one_block( c - p + i, c - p, 1 );
          one_block( c + p - i, c + p, 1 );
          one_block( c + p, c - p + i, 1 );
          one_block( c - p, c + p - i, 1 );
        end loop;
      end;
      --
      procedure orient( c pls_integer, p pls_integer )
      is
      begin
        one_block( c - p - 1, c - p - 1, 1 );
        one_block( c - p - 1, c - p, 1 );
        one_block( c - p, c - p - 1, 1 );
        one_block( c + p + 1, c - p - 1, 1 );
        one_block( c + p + 1, c - p, 1 );
        one_block( c + p + 1, c + p, 1 );
      end;
      --
      procedure dmode( c pls_integer, p_mode tp_bits )
      is
        l_p pls_integer;
        l_w pls_integer;
        l_t pls_integer;
      begin
        if l_compact
        then
          l_w := 7;
          l_p := 3;
        else
          l_w := 10;
          l_p := 5;
        end if;
        for i in 0 .. l_w - 1
        loop
          l_t := case when not l_compact and i >= l_w / 2 then 1 else 0 end;
          one_block( c - l_p + i + l_t, c - l_p - 2, p_mode( i ) );
          one_block( c + l_p + 2, c - l_p + i + l_t, p_mode( i + l_w ) );
          one_block( c + l_p - i - l_t, c + l_p + 2, p_mode( i + 2 * l_w ) );
          one_block( c - l_p - 2, c + l_p - i - l_t, p_mode( i + 3 * l_w ) );
        end loop;
      end;
      --
    begin
      l_tmp_matrix_size := case when l_compact then 11 else 14 end + 4 * l_layers;
      if l_compact
      then
        l_matrix_size := l_tmp_matrix_size;
        init_matrix( l_matrix_size );
        l_center := trunc( l_matrix_size / 2 );
        for i in 0 .. l_matrix_size - 1
        loop
          l_remap( i ) := i;
        end loop;
      else
        l_matrix_size := l_tmp_matrix_size + 1 + 2 * trunc( ( l_tmp_matrix_size - 2 ) / 30 );
        init_matrix( l_matrix_size );
        l_center := trunc( l_matrix_size / 2 );
        l_tmp_center := trunc( l_tmp_matrix_size / 2 );
        line( l_center, 6 );
        for i in 0 .. l_tmp_center - 1
        loop
          l_offs := i + trunc( i / 15 );
          l_remap( l_tmp_center - i - 1 ) := l_center - l_offs - 1;
          l_remap( l_tmp_center + i  ) := l_center + l_offs + 1;
        end loop;
        --
        for i in - trunc( l_matrix_size / 32 ) .. trunc( l_matrix_size / 32 )
        loop
          for j in - trunc( l_center / 2 ) .. trunc( l_center / 2 )
          loop
            one_block( l_center + 2 * j, l_center + i * 16, 1 );
            one_block( l_center + i * 16, l_center + 2 * j, 1 );
          end loop;
        end loop;
      end if;
      --
      one_block( l_center, l_center, 1 );
      line( l_center, 2 );
      line( l_center, 4 );
      orient( l_center, case when l_compact then 4 else 6 end );
      dmode( l_center, l_mode_msg );
      --
      l_row_offs := 0;
      for i in 0 .. l_layers - 1
      loop
        l_row_size := case when l_compact then 9 else 12 end + 4 * ( l_layers - i );
        for j in 0 .. l_row_size - 1
        loop
          for k in 0 .. 1
          loop
            one_block( l_remap( 2 * i + k )
                     , l_remap( 2 * i + j )
                     , l_sbits( l_row_offs + 2 * j + k )
                     );
            one_block( l_remap( 2 * i + j )
                     , l_remap( l_tmp_matrix_size - 1 - 2 * i - k )
                     , l_sbits( l_row_offs + 2 * j + k + 2 * l_row_size )
                     );
            one_block( l_remap( l_tmp_matrix_size - 1 - 2 * i - k )
                     , l_remap( l_tmp_matrix_size - 1 - 2 * i - j )
                     , l_sbits( l_row_offs + 2 * j + k + 4 * l_row_size )
                     );
            one_block( l_remap( l_tmp_matrix_size - 1 - 2 * i - j )
                     , l_remap( 2 * i + k )
                     , l_sbits( l_row_offs + 2 * j + k + 6 * l_row_size )
                     );
         end loop;
        end loop;
        l_row_offs := l_row_offs + 8 * l_row_size;
      end loop;
      --
    end;
  begin
    if (   (   isnchar( p_val )
           and utl_i18n.raw_to_nchar( utl_i18n.string_to_raw( p_val, 'WE8ISO8859P1' ), 'WE8ISO8859P1' ) != p_val
           )
        or (   not isnchar( p_val )
           and utl_i18n.raw_to_char( utl_i18n.string_to_raw( p_val, 'WE8ISO8859P1' ), 'WE8ISO8859P1' ) != p_val
           )
        )
    then
      append_bits( l_bits, 0, 5 );  -- PS
      append_bits( l_bits, 0, 5 );  -- FLG(n)
      append_bits( l_bits, 2, 3 );  -- FLG(2)
      append_bits( l_bits, 4, 4 );  -- 2
      append_bits( l_bits, 8, 4 );  -- 6 ECE #000026 UTF-8
      if check_neg( p_parm, 'double_backslash' )
      then
        get_bits( utl_i18n.string_to_raw( p_val, 'AL32UTF8' ), l_bits );
      else
        get_bits( utl_i18n.string_to_raw( replace( p_val, '\', '\\' ), 'AL32UTF8' ), l_bits );
      end if;
    else
      get_bits( utl_i18n.string_to_raw( p_val, 'WE8ISO8859P1' ), l_bits );
    end if;
    --
    declare
      l_tmp pls_integer;
    begin
      l_ecc_factor := 0.33;
      l_tmp := trunc( to_number( xjv( p_parm, 'ecc_pct' ) ) );
      if l_tmp between 1 and 100
      then
        l_ecc_factor := l_tmp / 100;
      end if;
    exception when others then null;
    end;
    --
    l_total_size := l_bits.count;
    l_ecc_size := trunc( l_total_size * l_ecc_factor ) + 11;
    l_total_size := l_total_size + l_ecc_size;
    l_wordsize := 0;
    --
    for i in 0 .. 33
    loop
      if i > 32
      then
        raise_application_error( -20000, 'Data too large for an Aztec code' );
      end if;
      l_compact := i <= 3;
      l_layers := case when l_compact then i + 1 else i end;
      l_bil := case when l_compact then 88 else 112 end;
      l_bil := ( l_bil + 16 * l_layers ) * l_layers;
      if l_total_size <= l_bil
      then
        if l_wordsize != case
                           when l_layers > 22 then 12
                           when l_layers > 8 then 10
                           when l_layers > 2 then 8
                           else 6
                         end
        then
          l_wordsize := case
                          when l_layers > 22 then 12
                          when l_layers > 8 then 10
                          when l_layers > 2 then 8
                          else 6
                        end;
          declare
            i pls_integer := l_bits.first;
            l_last pls_integer := l_bits.last;
            l_word number;
            l_mask number := power( 2, l_wordsize ) - 2;
          begin
            l_sbits.delete;
            loop
              exit when i > l_last;
              l_word := 0;
              for j in 0 .. l_wordsize - 1
              loop
                l_word := l_word * 2;
                if i + j > l_last
                then
                  l_word := l_word + 1;
                else
                  l_word := l_word + l_bits( i + j );
                end if;
              end loop;
              case bitand( l_word, l_mask )
                when l_mask
                then
                  l_word := l_mask;
                  i := i - 1;
                when 0
                then
                  l_word := l_word + 1 - bitand( l_word, 1 );
                  i := i - 1;
                else
                  null;
              end case;
              for j in reverse 0 .. l_wordsize - 1
              loop
                l_sbits( l_sbits.count ) := sign( bitand( l_word, power( 2, j ) ) );
              end loop;
              i := i + l_wordsize;
            end loop;
          end;
        end if;
        exit when (  ( not l_compact or l_sbits.count <= l_wordsize * 64 )
                  and l_sbits.count + l_ecc_size <= l_bil - mod( l_bil, l_wordsize )
                  );
      end if;
    end loop;
    --
    declare
      t_sz pls_integer;
      t_tmp tp_bits;
    begin
      t_sz := l_sbits.count / l_wordsize;
      if l_compact
      then
        append_bits( l_mode_msg, l_layers - 1, 2 );
        append_bits( l_mode_msg, t_sz - 1, 6 );
        t_tmp := reed_solomon( bitstoword( l_mode_msg, 4 ), 19, 16, 5 );
      else
        append_bits( l_mode_msg, l_layers - 1, 5 );
        append_bits( l_mode_msg, t_sz - 1, 11 );
        t_tmp := reed_solomon( bitstoword( l_mode_msg, 4 ), 19, 16, 6 );
      end if;
      for i in t_tmp.first .. t_tmp.last
      loop
        append_bits( l_mode_msg, t_tmp( i ), 4 );
      end loop;
      --
      t_sz := trunc( l_bil / l_wordsize ) - trunc( l_sbits.count  / l_wordsize );
      case l_wordsize
        when 6 then
          t_tmp := reed_solomon( bitstoword( l_sbits, 6 ), 67, 64, t_sz );
        when 8 then
          t_tmp := reed_solomon( bitstoword( l_sbits, 8 ), 301, 256, t_sz );
        when 10 then
          t_tmp := reed_solomon( bitstoword( l_sbits, 10 ), 1033, 1024, t_sz );
        when 12 then
          t_tmp := reed_solomon( bitstoword( l_sbits, 12 ), 4201, 4096, t_sz );
      end case;
      --
      if mod( l_bil, l_wordsize ) > 0
      then
        declare
          l_pad pls_integer := mod( l_bil, l_wordsize );
        begin
          for i in reverse l_sbits.first .. l_sbits.last
          loop
            l_sbits( i + l_pad ) := l_sbits( i );
          end loop;
          for i in l_sbits.first .. l_sbits.first + l_pad - 1
          loop
            l_sbits( i ) := 0;
          end loop;
        end;
      end if;
      for i in t_tmp.first .. t_tmp.last
      loop
        append_bits( l_sbits, t_tmp( i ), l_wordsize );
      end loop;
    end;
    --
    get_matrix( p_matrix );
    --
    add_quiet( p_matrix, p_parm, 0 );
    --
  end gen_aztec_matrix;
  --
  procedure gen_datamatrix_matrix( p_val varchar2 character set any_cs
                                 , p_parm varchar2
                                 , p_matrix out tp_matrix
                                 )
  is
    l_val raw(32767);
    l_ecc       tp_bits;
    l_stream    tp_bits;
    l_block     tp_bits;
    l_block_ecc tp_bits;
    l_chr pls_integer;
    l_idx pls_integer;
    l_len pls_integer;
    l_tmp pls_integer;
    l_mx  pls_integer;
    l_my  pls_integer;
    l_bc  pls_integer;
    l_ds  pls_integer;
    l_es  pls_integer;
    l_data tp_matrix;
    type tp_config is table of pls_integer;
    l_config tp_config;
    procedure get_config( p_config out tp_config, p_needed pls_integer, p_parm varchar2 )
    is
      type tp_dm_config is table of tp_config;
      l_dm_config tp_dm_config;
      l_idx pls_integer;
    begin
      l_dm_config := tp_dm_config
                       ( tp_config( 1, 10, 10, 1, 1, 5, 1, 3 )
                       , tp_config( 1, 12, 12, 1, 1, 7, 1, 5 )
                       , tp_config( 2, 8, 18, 1, 1, 7, 1, 5 )
                       , tp_config( 1, 14, 14, 1, 1, 10, 1, 8 )
                       , tp_config( 2, 8, 32, 1, 2, 11, 1, 10 )
                       , tp_config( 1, 16, 16, 1, 1, 12, 1, 12 )
                       , tp_config( 2, 12, 26, 1, 1, 14, 1, 16 )
                       , tp_config( 1, 18, 18, 1, 1, 14, 1, 18 )
                       , tp_config( 1, 20, 20, 1, 1, 18, 1, 22 )
                       , tp_config( 2, 12, 36, 1, 2, 18, 1, 22 )
                       , tp_config( 1, 22, 22, 1, 1, 20, 1, 30 )
                       , tp_config( 2, 16, 36, 1, 2, 24, 1, 32 )
                       , tp_config( 1, 24, 24, 1, 1, 24, 1, 36 )
                       , tp_config( 1, 26, 26, 1, 1, 28, 1, 44 )
                       , tp_config( 2, 16, 48, 1, 2, 28, 1, 49 )
                       , tp_config( 1, 32, 32, 2, 2, 36, 1, 62 )
                       , tp_config( 1, 36, 36, 2, 2, 42, 1, 86 )
                       , tp_config( 1, 40, 40, 2, 2, 48, 1, 114 )
                       , tp_config( 1, 44, 44, 2, 2, 56, 1, 144 )
                       , tp_config( 1, 48, 48, 2, 2, 68, 1, 174 )
                       , tp_config( 1, 52, 52, 2, 2, 84, 2, 204 )
                       , tp_config( 1, 64, 64, 4, 4, 112, 2, 280 )
                       , tp_config( 1, 72, 72, 4, 4, 144, 4, 368 )
                       , tp_config( 1, 80, 80, 4, 4, 192, 4, 456 )
                       , tp_config( 1, 88, 88, 4, 4, 224, 4, 576 )
                       , tp_config( 1, 96, 96, 4, 4, 272, 4, 696 )
                       , tp_config( 1, 104, 104, 4, 4, 336, 6, 816 )
                       , tp_config( 1, 120, 120, 6, 6, 408, 6, 1050 )
                       , tp_config( 1, 132, 132, 6, 6, 496, 8, 1304 )
                       , tp_config( 1, 144, 144, 6, 6, 620, 10, 1558 ) );
      l_idx := l_dm_config.first;
      loop
        exit when l_idx is null;
        if l_dm_config( l_idx )( 8 ) >= p_needed
        then
          p_config := l_dm_config( l_idx );
          exit;
        end if;
        l_idx := l_dm_config.next( l_idx );
      end loop;
      l_dm_config.delete;
    end;
  function hextobits( p_x varchar2 )
  return tp_bits
  is
    rv tp_bits;
  begin
    for i in 0 .. length( p_x ) / 2 - 1
    loop
      rv( i ) := to_number( substr( p_x, i * 2 + 1, 2 ), 'xx' );
    end loop;
    return rv;
  end;
  function bitstohex( p_x tp_bits )
  return varchar2
  is
    rv varchar2(32000);
  begin
    for i in p_x.first .. p_x.last
    loop
      rv := rv || to_char( p_x(i), 'fm0X' );
    end loop;
    return rv;
  end;
  --
  procedure place( p_stream  tp_bits
                 , p_numrows pls_integer
                 , p_numcols pls_integer
                 , p_data out tp_matrix
                 )
  is
    l_pos pls_integer;
    l_row pls_integer;
    l_col pls_integer;
    --
    procedure module( p_row pls_integer
                    , p_col pls_integer
                    , p_val pls_integer
                    , p_bit pls_integer
                    )
    is
      l_row pls_integer := p_row;
      l_col pls_integer := p_col;
    begin
      if l_row < 0
      then
        l_row := l_row + p_numrows;
        l_col := l_col + 4 - mod( p_numrows + 4, 8 );
      end if;
      if l_col < 0
      then
        l_col := l_col + p_numcols;
        l_row := l_row + 4 - mod( p_numcols + 4, 8 );
      end if;
      p_data( l_col )( l_row ) := sign( bitand( p_val, power( 2, 8 - p_bit ) ) );
    end;
    --
    procedure utah( p_row pls_integer
                  , p_col pls_integer
                  , p_val pls_integer
                  )
    is
    begin
      module( p_row - 2, p_col - 2, p_val, 1);
      module( p_row - 2, p_col - 1, p_val, 2);
      module( p_row - 1, p_col - 2, p_val, 3);
      module( p_row - 1, p_col - 1, p_val, 4);
      module( p_row - 1, p_col, p_val, 5);
      module( p_row, p_col - 2, p_val, 6);
      module( p_row, p_col - 1, p_val, 7);
      module( p_row, p_col, p_val, 8);
    end;
    --
    procedure corner1( p_val pls_integer )
    is
    begin
      module( p_numrows - 1, 0, p_val, 1);
      module( p_numrows - 1, 1, p_val, 2);
      module( p_numrows - 1, 2, p_val, 3);
      module( 0, p_numcols - 2, p_val, 4);
      module( 0, p_numcols - 1, p_val, 5);
      module( 1, p_numcols - 1, p_val, 6);
      module( 2, p_numcols - 1, p_val, 7);
      module( 3, p_numcols - 1, p_val, 8);
    end;
    --
    procedure corner2( p_val pls_integer )
    is
    begin
      module( p_numrows - 3, 0, p_val, 1);
      module( p_numrows - 2, 0, p_val, 2);
      module( p_numrows - 1, 0, p_val, 3);
      module( 0, p_numcols - 4, p_val, 4);
      module( 0, p_numcols - 3, p_val, 5);
      module( 0, p_numcols - 2, p_val, 6);
      module( 0, p_numcols - 1, p_val, 7);
      module( 1, p_numcols - 1, p_val, 8);
    end;
    --
    procedure corner3( p_val pls_integer )
    is
    begin
      module( p_numrows - 3, 0, p_val, 1);
      module( p_numrows - 2, 0, p_val, 2);
      module( p_numrows - 1, 0, p_val, 3);
      module( 0, p_numcols - 2, p_val, 4);
      module( 0, p_numcols - 1, p_val, 5);
      module( 1, p_numcols - 1, p_val, 6);
      module( 2, p_numcols - 1, p_val, 7);
      module( 3, p_numcols - 1, p_val, 8);
    end;
    --
    procedure corner4( p_val pls_integer )
    is
    begin
      module( p_numrows - 1, 0, p_val, 1);
      module( p_numrows - 1, p_numcols - 1, p_val, 2);
      module( 0, p_numcols - 3, p_val, 3);
      module( 0, p_numcols - 2, p_val, 4);
      module( 0, p_numcols - 1, p_val, 5);
      module( 1, p_numcols - 3, p_val, 6);
      module( 1, p_numcols - 2, p_val, 7);
      module( 1, p_numcols - 1, p_val, 8);
    end;
  begin
    l_pos := 0;
    l_row := 4;
    l_col := 0;
    loop
      if l_row = p_numrows and l_col = 0
      then
        corner1( p_stream( l_pos ) );
        l_pos := l_pos + 1;
      end if;
      if l_row = p_numrows - 2 and l_col = 0 and mod( p_numcols, 4 ) != 0
      then
        corner2( p_stream( l_pos ) );
        l_pos := l_pos + 1;
      end if;
      if l_row = p_numrows - 2 and l_col = 0 and mod( p_numcols, 8 ) = 4
      then
        corner3( p_stream( l_pos ) );
        l_pos := l_pos + 1;
      end if;
      if l_row = p_numrows + 4 and l_col = 2 and mod( p_numcols, 8 ) = 0
      then
        corner4( p_stream( l_pos ) );
        l_pos := l_pos + 1;
      end if;
      --
      loop
        if l_row < p_numrows and l_col >= 0 and not ( p_data.exists( l_col ) and p_data( l_col ).exists( l_row ) )
        then
--dbms_output.put_line( 'up: ' || l_row || ' ' || l_col || ' ' || l_pos );
          utah( l_row, l_col, p_stream( l_pos ) );
          l_pos := l_pos + 1;
        end if;
        l_row := l_row - 2;
        l_col := l_col + 2;
        exit when l_row < 0 or l_col >= p_numcols;
      end loop;
      l_row := l_row + 1;
      l_col := l_col + 3;
      --
      loop
        if l_row >= 0 and l_col < p_numcols and not ( p_data.exists( l_col ) and p_data( l_col ).exists( l_row ) )
        then
--dbms_output.put_line( 'down: ' || l_row || ' ' || l_col || ' ' || l_pos );
          utah( l_row, l_col, p_stream( l_pos ) );
          l_pos := l_pos + 1;
        end if;
        l_row := l_row + 2;
        l_col := l_col - 2;
        exit when l_row >= p_numrows or l_col < 0;
      end loop;
      l_row := l_row + 3;
      l_col := l_col + 1;
      --
      exit when l_row >= p_numrows and l_col >= p_numcols;
    end loop;
--dbms_output.put_line( 'pos: ' || l_pos );
    --
    if not ( p_data.exists( p_numcols - 1 ) and p_data( p_numcols - 1 ).exists( p_numrows - 1 ) )
    then
      p_data( p_numcols - 1 )( p_numrows - 1 ) := 1;
      p_data( p_numcols - 1 )( p_numrows - 2 ) := 0;
      p_data( p_numcols - 2 )( p_numrows - 1 ) := 0;
      p_data( p_numcols - 2 )( p_numrows - 2 ) := 1;
    end if;
  end place;
  --
  begin
    if (   (   isnchar( p_val )
           and utl_i18n.raw_to_nchar( utl_i18n.string_to_raw( p_val, 'US7ASCII' ), 'US7ASCII' ) = p_val
       )
       or (   not isnchar( p_val )
          and utl_i18n.raw_to_char( utl_i18n.string_to_raw( p_val, 'US7ASCII' ), 'US7ASCII' ) = p_val
          )
       )
    then
      l_val := utl_i18n.string_to_raw( p_val, 'US7ASCII' );
    else
      l_stream( 0 ) := 241; --Extended Channel Interpretation character
      l_stream( 1 ) := 27;  -- UTF-8 + 1
      l_val := utl_i18n.string_to_raw( p_val, 'AL32UTF8' );
    end if;
    l_len := nvl( utl_raw.length( l_val ), 0 );
    l_idx := 1;
    loop
      exit when l_idx > l_len;
      l_chr := to_number( utl_raw.substr( l_val, l_idx, 1 ), 'XX' );
      if l_chr < 128
      then
        if l_chr between 48 and 57
          and l_idx < l_len
          and utl_raw.substr( l_val, l_idx + 1, 1 ) between '30' and '39'
        then
          l_tmp := to_number( utl_i18n.raw_to_char(utl_raw.substr( l_val, l_idx, 2 ), 'US7ASCII' ) );
          l_stream( l_stream.count ) := 130 + l_tmp;
          l_idx := l_idx + 1;
        else
          l_stream( l_stream.count ) := l_chr + 1;
        end if;
      else
        l_stream( l_stream.count ) := 235;  -- set high bit for next character
        l_stream( l_stream.count ) := l_chr - 127;
      end if;
      l_idx := l_idx + 1;
    end loop;
    --
   get_config( l_config, l_stream.count, p_parm );
dbms_output.put_line( l_stream.count || ' ' || l_config( 8 ) || ' ' || bitstohex( l_stream ) );
dbms_output.put_line( case l_config( 1 )
                        when 2 then 'Rectangular'
                        else 'Square'
                      end || ' Symbol: '
|| 'data region ' || ( l_config( 3 ) / l_config( 5 ) - 2 ) || 'x' || ( l_config( 2 ) / l_config( 4 ) - 2 )
|| ', symbol size ' || l_config( 3 ) || 'x' || l_config( 2 )
|| ', symbol data size ' || ( l_config( 3 ) - 2 * l_config( 5 ) ) || 'x' || ( l_config( 2 ) - 2 * l_config( 4 ) )
|| ', codewords ' || l_config( 8 ) || '+' || l_config( 6 ) );
    if l_stream.count < nvl( l_config( 8 ), 0 )
    then
      l_stream( l_stream.count ) := 129;  -- End of message
      while l_stream.count < l_config( 8 ) loop
        l_tmp :=  mod( 149 * ( l_stream.count + 1 ), 253 ) + 1;
        l_stream( l_stream.count ) := mod( 129 + l_tmp, 254 );  -- 253-state padding
      end loop;
    end if;
 dbms_output.put_line( bitstohex( l_stream ) );
    if l_config( 7 ) = 1
    then
      l_ecc := reed_solomon( l_stream, 301, 256, l_config( 6 ) );
    else
      l_bc := l_config( 7 );
      l_ds := l_config( 8 ) / l_bc;
      l_es := l_config( 6 ) / l_bc;
      for b in 0 .. l_bc - 1
      loop
        if l_bc = 10 and b = 8
        then
          l_ds := 155;
        end if;
        l_block.delete;
        for i in 0 .. l_ds - 1
        loop
          l_block( i ) := l_stream( b + i * l_bc );
        end loop;
        l_block_ecc := reed_solomon( l_block, 301, 256, l_es );
        for i in 0 .. l_es - 1
        loop
          l_ecc( b + i * l_bc ) := l_block_ecc( i );
        end loop;
      end loop;
    end if;
      for i in l_ecc.first .. l_ecc.last
      loop
        l_stream( l_stream.count ) := l_ecc( i );
      end loop;
dbms_output.put_line( bitstohex( l_stream ) );
    --
    place( l_stream
         , l_config( 2 ) - 2 * l_config( 4 )
         , l_config( 3 ) - 2 * l_config( 5 )
         , l_data
         );
    --
    l_my := 0;
    for y in 0 .. l_config( 2 ) - 2 * l_config( 4 ) - 1
    loop
      if mod( y, l_config( 2 ) / l_config( 4 ) - 2 ) = 0
      then
        for x in 0 .. l_config( 3 ) - 1
        loop
          p_matrix( x )( l_my ) := 1 - mod( x, 2 );
        end loop;
        l_my := l_my + 1;
      end if;
      l_mx := 0;
      for x in 0 .. l_config( 3 ) - 2 * l_config( 5 ) - 1
      loop
        if mod( x, l_config( 3 ) / l_config( 5 ) - 2 ) = 0
        then
          p_matrix( l_mx )( l_my ) := 1;
          l_mx := l_mx + 1;
        end if;
        p_matrix( l_mx )( l_my ) := l_data( x )( y );
        l_mx := l_mx + 1;
        if mod( x + 1, l_config( 3 ) / l_config( 5 ) - 2 ) = 0
        then
          p_matrix( l_mx )( l_my ) := 1 - mod( y, 2 );
          l_mx := l_mx + 1;
        end if;
      end loop;
      l_my := l_my + 1;
      if mod( y + 1, l_config( 2 ) / l_config( 4 ) - 2 ) = 0
      then
        for x in 0 .. l_config( 3 ) - 1
        loop
          p_matrix( x )( l_my ) := 1;
        end loop;
        l_my := l_my + 1;
      end if;
    end loop;
    --
    l_data.delete;
    --
    add_quiet( p_matrix, p_parm, 2 );
    --
  end gen_datamatrix_matrix;
  --
  procedure gen_code128( p_val varchar2 character set any_cs
                       , p_parm varchar2
                       , p_row in out nocopy tp_bar_row
                       , p_height out number
                       , p_human out varchar2 character set any_cs
                       )
  is
    l_quiet tp_bar_row;
    l_code pls_integer;
    l_map varchar2(400);
    l_mapping tp_mapping;
    l_check number;
    l_idx pls_integer;
    l_buf raw(32767);
    l_char pls_integer;
    l_charset varchar2(100);
  begin
    p_human := null;
    p_height := null;
    p_row := tp_bar_row();
    --
    l_map := '4555455541161252151521612515125216110591491580951851945905095184'
          || '9158184881590591485194195044646464402620622406224226042260262004'
          || 'A0682480860A42848844286084824A0488806824A04842860A408C0530E00017'
          || '035107134305314053071143170341350710503C807012C001D10D11C0D11C11'
          || 'D0C11D01D1044C4C4C4400E02C20C0C20E0C02C2008C0C880CC08431413419';
    for i in 0 .. 105
    loop
      l_code := to_number( substr( l_map, 1 + i * 3, 3 ), 'XXX' );
      l_mapping( i ) := tp_bar_row
          ( 2 * sign( bitand( l_code, 2048 ) ) + sign( bitand( l_code, 1024 ) ) + 1
          , - ( 2 * sign( bitand( l_code, 512 ) ) + sign( bitand( l_code, 256 ) ) + 1 )
          , 2 * sign( bitand( l_code, 128 ) ) + sign( bitand( l_code, 64 ) ) + 1
          , - ( 2 * sign( bitand( l_code, 32 ) ) + sign( bitand( l_code, 16 ) ) + 1 )
          , 2 * sign( bitand( l_code, 8 ) ) + sign( bitand( l_code, 4 ) ) + 1
          , - ( 2 * sign( bitand( l_code, 2 ) ) + sign( bitand( l_code, 1 ) ) + 1 )
          );
    end loop;
    --
    l_idx := 1;
    l_quiet := tp_bar_row( -10 );
    p_row := l_quiet;
    if p_val is not null and ltrim( p_val, '1234567890' ) is null
    then
      l_check := 105; -- Start Code C
      p_row := p_row multiset union l_mapping( l_check );
      for c in 1 .. trunc( length( p_val ) / 2 )
      loop
        l_char := to_number( substr( p_val, 2 * c - 1, 2 ) );
        p_row := p_row multiset union l_mapping( l_char );
        l_check := l_check + l_char * l_idx;
        l_idx := l_idx + 1;
      end loop;
      if mod( length( p_val ), 2 ) = 1
      then
        p_row := p_row multiset union l_mapping( 100 ); -- Switch B
        l_check := l_check + 100 * l_idx;
        l_idx := l_idx + 1;
        l_char := ascii( substr( p_val, -1 ) ) - 32;
        p_row := p_row multiset union l_mapping( l_char );
        l_check := l_check + l_char * l_idx;
      end if;
      p_human := p_val;
    else
      if jv( p_parm, 'charset' ) like '%1252%'
      then
        l_charset := 'WE8MSWIN1252';
      else
        l_charset := 'WE8ISO8859P1';
      end if;
      l_buf := utl_i18n.string_to_raw( p_val, l_charset );
      --
      l_check := 104; -- Start Code B
      p_row := p_row multiset union l_mapping( l_check );
      for c in 1 .. utl_raw.length( l_buf )
      loop
        l_char := to_number( utl_raw.substr( l_buf, c, 1 ), 'xx' );
        if l_char > 127
        then
          p_human := p_human || utl_i18n.raw_to_char( to_char( l_char, 'fm0X' ), l_charset );
          p_row := p_row multiset union l_mapping( 100 ); -- FNC 4
          l_check := l_check + 100 * l_idx;
          l_char := l_char - 128;
          l_idx := l_idx + 1;
        end if;
        if l_char between 32 and 127
        then
          p_human := p_human || utl_i18n.raw_to_char( to_char( l_char, 'fm0X' ), l_charset );
          p_row := p_row multiset union l_mapping( l_char - 32 );
          l_check := l_check + ( l_char - 32 ) * l_idx;
        elsif l_char < 32
        then
          p_row := p_row multiset union l_mapping( 98 ); -- Shift A
          l_check := l_check + 98 * l_idx;
          l_idx := l_idx + 1;
          p_row := p_row multiset union l_mapping( l_char );
          l_check := l_check + l_char * l_idx;
        end if;
        l_idx := l_idx + 1;
      end loop;
    end if;
    p_row := p_row multiset union l_mapping( mod( l_check, 103 ) );
    p_row := p_row multiset union tp_bar_row( 2, -3, 3, -1, 1, -1, 2 ); -- stop
    p_row := p_row multiset union l_quiet;
    p_height := greatest( 20, p_row.count / 5 );
    l_quiet.delete;
  exception
    when others then
      l_quiet.delete;
  end gen_code128;
  --
  procedure gen_code39( p_val varchar2
                      , p_parm varchar2
                      , p_row in out nocopy tp_bar_row
                      , p_height out number
                      , p_human out varchar2
                      )
  is
    l_val varchar2(4000);
    l_ascii pls_integer;
    l_quiet tp_bar_row;
    l_mapping tp_mapping;
    --
    procedure add2line( p_row in out nocopy tp_bar_row, p_val tp_bar_row )
    is
    begin
      p_row := p_row multiset union tp_bar_row( -1 ) -- one separator
                     multiset union p_val;
    end;
    --
    procedure add2mapping( p_mapping in out nocopy tp_mapping
                         , p_idx pls_integer
                         , p_a pls_integer
                         , p_b pls_integer
                         )
    is
    begin
      p_mapping( p_idx ) := p_mapping( p_a )
             multiset union tp_bar_row( -1 )
             multiset union p_mapping( p_b );
    end;
    --
    procedure create_mapping( p_mapping in out nocopy tp_mapping
                            , p_map varchar2
                            )
    is
      l_entry pls_integer;
      l_bits tp_bar_row;
      function wide_narrow( p_bit pls_integer )
      return number
      is
      begin
        return case when p_bit = 0 then 1 else 2.4 end;
      end;
    begin
      for i in 0 .. length( p_map ) / 4 - 1
      loop
        l_entry := to_number( substr( p_map, i * 4 + 1, 4 ), '0XXX' );
        l_bits := tp_bar_row( wide_narrow( bitand( l_entry, 256 ) ) );
        for i in reverse 0 .. 7
        loop
          l_bits := l_bits multiset union
                 tp_bar_row( ( 1 - 2 * mod( i, 2 ) )
                           * wide_narrow( bitand( l_entry, power( 2, i ) ) )
                           );
        end loop;
        p_mapping( nvl( nullif( trunc( l_entry / 512 ), 0 ), 128 ) ) := l_bits;
      end loop;
      l_bits.delete;
    end;
  begin
    p_human := null;
    p_height := null;
    p_row := tp_bar_row();
    l_val := p_val;
    if check_pos( p_parm, 'upper' )
    then
      l_val := upper( l_val );
    end if;
    --
    l_quiet := tp_bar_row( -10 );
    create_mapping( l_mapping
                  , '009440C448A84A2A568A5A855D845EA2603463216461676068316B306C70'
                 || '6E257124726483098449874888198B188C588E0D910C924C941C97039843'
                 || '9B429C139F12A052A207A506A646A816AB81ACC1AFC0B091B390B4D0'
                  );
    --
    if check_pos( p_parm, 'full' )
     or check_pos( p_parm, 'extended' )
    then
      for i in 97 .. 122 loop
        add2mapping( l_mapping, i, 43, i - 32 ); -- +
      end loop;
      for i in 1 .. 26 loop
        add2mapping( l_mapping, i, 36, i + 64 ); -- $
      end loop;
      for i in 1 .. 5 loop
        add2mapping( l_mapping, 26 + i, 37, i + 64 ); -- %
        add2mapping( l_mapping, 26 + i + 32, 37, i + 64 + 5 );  -- %
        add2mapping( l_mapping, 26 + i + 64, 37, i + 64 + 10 ); -- %
        add2mapping( l_mapping, 26 + i + 96, 37, i + 64 + 15 ); -- %
      end loop;
      add2mapping( l_mapping, 0, 37, 85 );   -- %U
      add2mapping( l_mapping, 64, 37, 86 );  -- %V
      add2mapping( l_mapping, 96, 37, 87 );  -- %W
      for i in 33 .. 44 loop
        add2mapping( l_mapping, i, 47, i + 32 ); -- /
      end loop;
      add2mapping( l_mapping, 47, 47, 79 ); -- /O
    end if;
    --
    p_row := l_quiet;
    add2line( p_row, l_mapping( 128 ) ); -- start character
    for i in 1 .. length( l_val )
    loop
      -- characters not allowed for (extended) code39 will throw an error
      -- either on ascii not fitting in a plsql_integer or an uninitialized mapping
      l_ascii := ascii( substr( l_val, i, 1 ) );
      add2line( p_row, l_mapping( l_ascii ) );
      if l_ascii between 32 and 126
      then
        p_human := p_human || chr( l_ascii );
      end if;
    end loop;
    add2line( p_row, l_mapping( 128 ) ); -- end character
    add2line( p_row, l_quiet );
    --
    p_height := greatest( 20, p_row.count / 5 );
    p_human := '*' || p_human || '*';
    --
    l_quiet.delete;
    l_mapping.delete;
  exception
    when others then
      l_quiet.delete;
      l_mapping.delete;
  end gen_code39;
  --
  procedure gen_itf( p_val varchar2
                   , p_parm varchar2
                   , p_row in out nocopy tp_bar_row
                   , p_height out number
                   , p_human out varchar2
                   )
  is
    c_wide constant number := 2;
    l_quiet tp_bar_row;
    l_idx1 pls_integer;
    l_idx2 pls_integer;
    l_def varchar2(50) := 'nnWWnWnnnWnWnnWWWnnnnnWnWWnWnnnWWnnnnnWWWnnWnnWnWn';
  begin
    p_human := null;
    p_height := null;
    p_row := tp_bar_row();
    if ltrim( p_val, '1234567890' ) is not null
    then
      return;
    end if;
    p_human := p_val;
    if check_pos( p_parm, 'checksum' )
    then
      if mod( length( p_human ), 2 ) = 0
      then
        p_human := '0' || p_human;
      end if;
      p_human := p_human || upc_checksum( p_human );
    else
      if mod( length( p_human ), 2 ) = 1
      then
        p_human := '0' || p_human;
      end if;
    end if;
    --
    l_quiet := tp_bar_row( -10 );
    p_row := l_quiet;
    p_row := p_row multiset union tp_bar_row( 1, -1, 1, -1 ); -- start code
    for c in 1 .. length( p_human ) / 2
    loop
      l_idx1 := substr( p_human, c * 2 - 1, 1 );
      l_idx2 := substr( p_human, c * 2, 1 );
      for i in 1 .. 5
      loop
        p_row := p_row multiset union
          tp_bar_row( case substr( l_def, l_idx1 * 5 + i, 1 )
                        when 'n' then 1 else c_wide
                      end
                    , - case substr( l_def, l_idx2 * 5 + i, 1 )
                          when 'n' then 1 else c_wide
                        end
                    );
      end loop;
    end loop;
    p_row := p_row multiset union tp_bar_row( c_wide, -1, 1 ); -- stop code
    p_row := p_row multiset union l_quiet;
    p_height := greatest( 20, p_row.count / 5 );
    --
    if check_pos( p_parm, 'bearer' )
    then
      p_row := tp_bar_row( 4 ) multiset union p_row multiset union tp_bar_row( 4 );
    end if;
    --
    l_quiet.delete;
  exception
    when others then
      l_quiet.delete;
  end gen_itf;
  --
  procedure init_ean_digits( p_mapping in out nocopy tp_mapping )
  is
    l_w1 number;
    l_w2 number;
    l_w3 number;
    l_w4 number;
  begin
    -- number Set C
    p_mapping( 20 ) := tp_bar_row( 3, -2, 1, -1 );
    p_mapping( 21 ) := tp_bar_row( 2.0769, -1.9231, 2.0769, -0.9231 );
    p_mapping( 22 ) := tp_bar_row( 2.0769, -0.9231, 2.0769, -1.9231 );
    p_mapping( 23 ) := tp_bar_row( 1, -4, 1, -1 );
    p_mapping( 24 ) := tp_bar_row( 1, -1, 3, -2 );
    p_mapping( 25 ) := tp_bar_row( 1, -2, 3, -1 );
    p_mapping( 26 ) := tp_bar_row( 1, -1, 1, -4 );
    p_mapping( 27 ) := tp_bar_row( 1.0769, -2.9231, 1.0769, -1.9231 );
    p_mapping( 28 ) := tp_bar_row( 1.0769, -1.9231, 1.0769, -2.9231 );
    p_mapping( 29 ) := tp_bar_row( 3, -1, 1, -2 );
    p_mapping( 30 ) := tp_bar_row( 1, -1, 1 );         -- left/right guard bar
    p_mapping( 31 ) := tp_bar_row( -1, 1, -1, 1, -1 ); -- centre guard bar
    p_mapping( 33 ) := tp_bar_row( 1, -1, 2 );         -- add-on guard bar
    p_mapping( 34 ) := tp_bar_row( -1, 1 );            -- add-on delineator
    --
    for i in 0 .. 9
    loop
      l_w1 := p_mapping( 20 + i )(1);
      l_w2 := p_mapping( 20 + i )(2);
      l_w3 := p_mapping( 20 + i )(3);
      l_w4 := p_mapping( 20 + i )(4);
      -- number Set A
      p_mapping( i ) := tp_bar_row( - l_w1, - l_w2, - l_w3, - l_w4 );
      -- number Set B
      p_mapping( 10 + i ) := tp_bar_row( l_w4, l_w3, l_w2, l_w1 );
    end loop;
  end;
  --
  -- 7x9 font, containing 0,1,2,3,4,5,6,7,8,9,<,>
  function number_font
  return raw
  is
    t_def varchar2(150) :=
      'H4sIAAAAAAAACyVMIRIAQQgqBqPhwkWfQdhA8FEXfPyJy4yIIqIuEO8TQqPIDLcV' ||
      'IAvym4M2szP9oEYoNevMCHcdbYk+pfaJR3JJww8KA+wEbAAAAA==';
    t_tmp raw(200);
    t_fnt raw(32767);
    t_line pls_integer;
  begin
    t_tmp := utl_compress.lz_uncompress( utl_encode.base64_decode( utl_raw.cast_to_raw( t_def ) ) );
    for c in 0 .. 11
    loop
      for i in 1 .. 9
      loop
        t_line := to_number( utl_raw.substr( t_tmp, c * 9 + i, 1 ), 'XX' );
        for j in 1 .. 7
        loop
          t_fnt := utl_raw.concat( t_fnt
                                 , case when bitand( t_line, power( 2, j ) ) = 0 then '01' else '00' end
                                 );
        end loop;
      end loop;
    end loop;
    return t_fnt;
  end;
--
-- a 8x14 font, containing codepage 1252, which happens to be a superset of ISO-8859-1
-- change to 8x15 font? https://github.com/ntwk/nixedsys-font/blob/master/nixedsys-normal.bdf
-- or 9x16 https://www.uwe-sieber.de/dosfon_e.html
  function cp1252_font
  return raw
  is
    t_def varchar2(2000) :=
      'H4sIAAAAAAAAC+1WMYslRRAehkEGWZZmogmWY3h08AKDh9EgzTKMzTIsi6yLl4k8' ||
      'FgODC+SiRYbBe6EYDMMlJ7LgworuPzgwMrjfYeAvMBI8Dquqq7ur3zvNBWv3bt73' ||
      'qqaqvqqurs2yVJQTeALQWksdwIl+NRmOyzyPVbWMaPv58y9KpZr3ri7Qsta63pxb' ||
      'az9wLqWXUjUkqkQEDxLV+CD1VMeoECV9G7/yptm0l7zLOiuKEnNpug6R6fuz86vr' ||
      'vjcUvV67gKesywuI3U0eneQ5W+bvvFut+inPcwrVdd0In53uBHyPfc+WYONCKvbS' ||
      'm6CjT6dg0MoUAydGxIgSgXoUkV0gSN+j3vuEeFlZ7vP/v3//vf6Nu9u7u7sfv/va' ||
      'c+j7Cf8jHTwpz5F1wKLjzG5sT2JvPL8b1E4J6sJ7H3lGIQBFaPnAKOJ3XJBYS6i3' ||
      'w+V2ezlYsuy8UITdq4dl2aGwz+ur8zP2aXoW4zn0o8iF5NzkQecjGEjThP698Zkp' ||
      'n3X0SZ/gHNc15YKyLA+vOBf8XmuXy25n7Tp4wU5jFxyHtmFpHT8403CaC+pfW7Kg' ||
      'Tq1t9laZMnU4gyY/xTPDNSOKrn9MsePzmZMdW7rSQN9Id0xzo+LksWVuhE/iBxab' ||
      '2D/I/Sh2EEPZ4Uuu7ibpNMjrhYSR8BnaZBLd6DoYcqG5yj7bhnOGOurg6DJjDses' ||
      '6xO2vesTI+zfsoxBp2sdcukjdyx5nPcjcN5ghCOOJ87LBlpXYP82AK6Xbw+6FOVk' ||
      '1f0JP6uTfzaBywHjVZ7SvI06rQdh6eoZ2qcEopuwTm7kLBt+usSb7pOH53h1hhHw' ||
      'lUeyfKNM8zx/D//mg1sT3pvkuf4XKZPb2NHyUiXboqr2Ms1qY2qJ01hv3OP9i/Sl' ||
      'vx4//jjJVJyQjNZHEXfHuCy3sJMO00aGvu/WxqE+tKTVFyfHbbl54do3zQ2M+4oz' ||
      'ty1suNaNNrhcQ4dG3zF8spfWdpvBWl0XMFR6vyrTsxf3P9y/eIY+28JYk8n8Kz3M' ||
      'g668LS8jL0Z4eXn/8ud79GKyRHAV71PELEfSlSqat0UdtlSWdNYJjdHZVzirE07b' ||
      '0wpE1G1fcK2pjawy1Svhh+SAIjRy26wgm9XZN4+AI6PmYinKR9lvalv9HnTco8bd' ||
      'L3B/mrjxIGuBYFYEgpMlkNapZS2Qm5XXOCu3VOawN3GYwnKcKJ5AEE8gnaBGhVXZ' ||
      '0nsCwXsCwfkUyO3pP3hPAwexKZk737TMnRFzZ8TcGelE53qLK89dmOZD9/fSr1Rd' ||
      'uTdLJRFE6BOfiWW6N3nl8AaAYzAgvv6UjkkpNx6yiAhZRIQsItI6tawFAnlSPYW7' ||
      '4Akj3pt4U7iq+U3pquaRq5pHOkFNsinLBG0GiYYErdVabEPgILdvKTdl6KDIRbwn' ||
      'kNbphqV5DoOWG2rgLxxB7E2IIFDooK/nWy2Tvxmog9EyN38DB5K36AAOAAA=';
    t_tmp raw(3999);
    t_fnt raw(32767);
    t_line pls_integer;
  begin
    t_tmp := utl_compress.lz_uncompress( utl_encode.base64_decode( utl_raw.cast_to_raw( t_def ) ) );
    for c in 0 .. 255
    loop
      for i in 1 .. 14
      loop
        t_line := to_number( utl_raw.substr( t_tmp, c * 14 + i, 1 ), 'XX' );
        for j in reverse 0 .. 7
        loop
          t_fnt := utl_raw.concat( t_fnt
                                 , case when bitand( t_line, power( 2, j ) ) = 0 then '01' else '00' end
                                 );
        end loop;
      end loop;
    end loop;
    return t_fnt;
  end;
  --
  function to_png_px( p_val tp_bar_row, p_scale number := 1 )
  return varchar2
  is
    l_rv varchar2(4000);
    l_w pls_integer;
  begin
    for i in p_val.first .. p_val.last
    loop
      l_w := round( abs( p_val( i ) ) * p_scale );
      l_rv := l_rv || case when p_val( i ) > 0
                            then rpad( '00', 2 * l_w, '00' )
                            else rpad( '01', 2 * l_w, '01' )
                          end;
    end loop;
    return l_rv;
  end;
  --
  function font_px( p_fnt raw
                  , p_fh pls_integer
                  , p_fw pls_integer
                  , p_idx pls_integer
                  , p_l pls_integer
                  , p_scale number
                  )
  return varchar2
  is
    l_px varchar2(100);
    l_br tp_bar_row := tp_bar_row();
  begin
    for c in 1 .. p_fw
    loop
      l_px := utl_raw.substr( p_fnt, ( p_idx * p_fh + p_l ) * p_fw + c, 1 );
      l_br := l_br multiset union tp_bar_row( 1 - 2 * sign( l_px ) );
    end loop;
    return to_png_px( l_br, p_scale );
  end;
  --
  function png_bar( p_row in out nocopy tp_bar_row
                  , p_parm   varchar2
                  , p_height number
                  , p_human  varchar2
                  )
  return raw
  is
    l_dat blob;
    c_human_sep constant number := 2;
    c_bearer    constant number := 4;
    l_scale number := 1;
    l_line varchar2(32767);
    l_height number;
    l_width number;
    l_fnt raw(32767);
    l_fh   pls_integer;
    l_fw   pls_integer;
    l_pre  pls_integer;
    l_post pls_integer;
  begin
    if nvl( p_height, 0 ) <= 0
    then
      return null;
    end if;
    --
    l_scale := nvl( check_int( p_parm, 'scale', 10 ), l_scale );
    --
    l_height := p_height * l_scale;
    l_line := '00' || to_png_px( p_row, l_scale ); -- filter type None
    l_width := ( length( l_line ) - 2 ) / 2;
    --
    dbms_lob.createtemporary( l_dat, true, dbms_lob.call );
    --
    if check_pos( p_parm, 'bearer' )
    then
      for i in 1 .. c_bearer * l_scale
      loop
        dbms_lob.writeappend( l_dat
                            , l_width + 1
                            , '00' || to_png_px( tp_bar_row( l_width ) )
                            );
      end loop;
    end if;
    --
    for i in 1 .. l_height
    loop
      dbms_lob.writeappend( l_dat, l_width + 1, l_line );
    end loop;
    --
    if check_pos( p_parm, 'bearer' )
    then
      for i in 1 .. c_bearer * l_scale
      loop
        dbms_lob.writeappend( l_dat
                            , l_width + 1
                            , '00' || to_png_px( tp_bar_row( l_width ) )
                            );
      end loop;
    end if;
    --
    l_fnt := cp1252_font;  -- 8x14
    l_fw := 8;
    l_fh := 14;
    l_post := l_width - l_fw * l_scale * length( p_human );
    l_pre :=  l_post / 2;
    l_post := l_post - l_pre;
    for j in 0 .. l_fh - 1
    loop
      l_line := '00' || to_png_px( tp_bar_row( - l_pre ) );
      for c in 1 .. length( p_human )
      loop
        l_line := l_line || font_px( l_fnt, l_fh, l_fw, ascii( substr( p_human, c ) ), j, l_scale );
      end loop;
      l_line := l_line || to_png_px( tp_bar_row( - l_post ) );
      for s in 1 .. l_scale
      loop
        dbms_lob.writeappend( l_dat, l_width + 1, l_line );
        l_height := l_height + 1;
      end loop;
    end loop;
    dbms_lob.writeappend( l_dat
                        , l_width + 1
                        , '00' || to_png_px( tp_bar_row( -l_width ) )
                        );
    --
    return generate_png( l_dat, l_width, l_height + l_fh + 1 + 2 * c_bearer * l_scale, p_parm );
  end png_bar;
  --
  function code39( p_val varchar2, p_parm varchar2 )
  return raw
  is
    l_row tp_bar_row;
    l_height number;
    l_human varchar2(4000);
  begin
    gen_code39( p_val, p_parm, l_row, l_height, l_human );
    return png_bar( l_row, p_parm, l_height, l_human );
  end code39;
  --
  function code128( p_val varchar2 character set any_cs, p_parm varchar2 )
  return raw
  is
    l_row tp_bar_row;
    l_height number;
    l_human varchar2(4000) character set p_val%charset;
  begin
    gen_code128( p_val, p_parm, l_row, l_height, l_human );
    return png_bar( l_row, p_parm, l_height, l_human );
  end;
--
  function itf( p_val varchar2, p_parm varchar2 )
  return raw
  is
    l_row tp_bar_row;
    l_height number;
    l_human varchar2(4000);
  begin
    gen_itf( p_val, p_parm, l_row, l_height, l_human );
    return png_bar( l_row, p_parm, l_height, l_human );
  end itf;
  --
  function ean8( p_val varchar2, p_parm varchar2 )
  return raw
  is
    l_val varchar2(100);
    l_mapping tp_mapping;
    l_scale number := 1;
    l_quiet tp_bar_row;
    l_dat blob;
    l_line varchar2(4000);
    l_width pls_integer;
    l_height pls_integer;
    l_fnt raw(1000);
    --
    function add_bars( p_val varchar2, p_set pls_integer )
    return varchar2
    is
    begin
      return to_png_px( l_mapping( p_set + substr( p_val, 1, 1 ) ), l_scale )
          || to_png_px( l_mapping( p_set + substr( p_val, 2, 1 ) ), l_scale )
          || to_png_px( l_mapping( p_set + substr( p_val, 3, 1 ) ), l_scale )
          || to_png_px( l_mapping( p_set + substr( p_val, 4, 1 ) ), l_scale );
    end;
    --
  begin
    --
    if ltrim( p_val, '1234567890' ) is not null or length( p_val ) > 8
    then
      return null;
    end if;
    if length( p_val ) < 8
    then
      l_val := lpad( p_val, 7, '0' ) || upc_checksum( p_val );
    else
      l_val := p_val;
    end if;
    --
    init_ean_digits( l_mapping );
    l_quiet := tp_bar_row( -7 );
    l_scale := nvl( check_int( p_parm, 'scale', 10 ), l_scale );
    --
    l_line := '00'; -- filter type None
    l_line := l_line || to_png_px( l_quiet, l_scale ); -- left quiet zone
    l_line := l_line || to_png_px( l_mapping( 30 ), l_scale ); -- left guard
    l_line := l_line || add_bars( substr( l_val, 1, 4 ), 0 );
    l_line := l_line || to_png_px( l_mapping( 31 ), l_scale ); -- centre guard
    l_line := l_line || add_bars( substr( l_val, 5, 4 ), 20 );
    l_line := l_line || to_png_px( l_mapping( 30 ), l_scale ); -- right guard
    l_line := l_line || to_png_px( l_quiet, l_scale ); -- right quiet zone
    --
    l_height := 55 * l_scale;
    l_width := ( length( l_line ) - 2 ) / 2;
    dbms_lob.createtemporary( l_dat, true, dbms_lob.call );
    for i in 1 .. l_height
    loop
      dbms_lob.writeappend( l_dat, l_width + 1, l_line );
    end loop;
    --
    l_fnt := number_font;
    l_line :=    '00' -- filter type None
              || to_png_px( l_quiet, l_scale ) -- left quiet zone
              || to_png_px( l_mapping( 30 ), l_scale ) -- left guard
              || to_png_px( tp_bar_row( -7 * 4 ), l_scale ) -- 4 numbers
              || to_png_px( l_mapping( 31 ), l_scale ) -- centre guard
              || to_png_px( tp_bar_row( -7 * 4 ), l_scale ) -- 4 numbers
              || to_png_px( l_mapping( 30 ), l_scale ) -- right guard
              || to_png_px( l_quiet, l_scale ); -- right quiet zone
    dbms_lob.writeappend( l_dat, l_width + 1, l_line );
    --
    for j in 0 .. 8
    loop
      l_line := '00'; -- filter type None
      l_line := l_line || font_px( l_fnt, 9, 7, 10, j, l_scale ); -- <
      l_line := l_line || to_png_px( case when j < 4 then l_mapping( 30 ) else tp_bar_row( -3 ) end, l_scale ); -- left guard
      for c in 1 .. 4
      loop
        l_line := l_line || font_px( l_fnt, 9, 7, substr( l_val, c, 1 ), j, l_scale );
      end loop;
      l_line := l_line || to_png_px( case when j < 4 then l_mapping( 31 ) else tp_bar_row( -5 ) end, l_scale ); -- centre guard
      for c in 5 .. 8
      loop
        l_line := l_line || font_px( l_fnt, 9, 7, substr( l_val, c, 1 ), j, l_scale );
      end loop;
      l_line := l_line || to_png_px( case when j < 4 then l_mapping( 30 ) else tp_bar_row( -3 ) end, l_scale ); -- right guard
      l_line := l_line || font_px( l_fnt, 9, 7, 11, j, l_scale ); -- >
      for s in 1 .. l_scale
      loop
        dbms_lob.writeappend( l_dat, l_width + 1, l_line );
        l_height := l_height + 1;
      end loop;
    end loop;
    --
    return generate_png( l_dat, l_width, l_height + 1, p_parm );
  end ean8;
  --
  function ean13( p_val varchar2, p_parm varchar2 )
  return raw
  is
    l_val varchar2(100);
    l_mapping tp_mapping;
    l_scale number := 1;
    l_dat blob;
    l_line varchar2(4000);
    l_width pls_integer;
    l_height pls_integer := 69;
    l_fnt raw(1000);
    --
    function add_bars( p_val varchar2, p_first pls_integer := null )
    return varchar2
    is
      l_rv varchar2(4000);
      l_set pls_integer;
    begin
      if p_first is null
      then
        for i in 1 .. 6
        loop
          l_rv := l_rv || to_png_px( l_mapping( 20 + substr( p_val, i, 1 ) ), l_scale );
        end loop;
      else
        l_set := 0;
        l_rv := l_rv|| to_png_px( l_mapping( l_set + substr( p_val, 1, 1 ) ), l_scale );
        l_set := case when p_first < 4 then 0 else 10 end;
        l_rv := l_rv|| to_png_px( l_mapping( l_set + substr( p_val, 2, 1 ) ), l_scale );
        l_set := case when p_first in ( 0, 4, 7, 8 ) then 0 else 10 end;
        l_rv := l_rv|| to_png_px( l_mapping( l_set + substr( p_val, 3, 1 ) ), l_scale );
        l_set := case when p_first in ( 0, 1, 4, 5, 9 ) then 0 else 10 end;
        l_rv := l_rv|| to_png_px( l_mapping( l_set + substr( p_val, 4, 1 ) ), l_scale );
        l_set := case when p_first in ( 0, 2, 5, 6, 7 ) then 0 else 10 end;
        l_rv := l_rv|| to_png_px( l_mapping( l_set + substr( p_val, 5, 1 ) ), l_scale );
        l_set := case when p_first in ( 0, 3, 6, 8, 9 ) then 0 else 10 end;
        l_rv := l_rv|| to_png_px( l_mapping( l_set + substr( p_val, 6, 1 ) ), l_scale );
      end if;
      return l_rv;
    end;
    --
  begin
    --
    if ltrim( p_val, '1234567890' ) is not null or length( p_val ) > 13
    then
      return null;
    end if;
    if length( p_val ) < 13
    then
      l_val := lpad( p_val, 12, '0' ) || upc_checksum( p_val );
    else
      l_val := p_val;
    end if;
    --
    init_ean_digits( l_mapping );
    --
    l_scale := nvl( check_int( p_parm, 'scale', 10 ), l_scale );
    --
    l_line := '00'; -- filter type None
    l_line := l_line || to_png_px( tp_bar_row( -11 ), l_scale ); -- left quiet zone
    l_line := l_line || to_png_px( l_mapping( 30 ), l_scale ); -- left guard
    l_line := l_line || add_bars( substr( l_val, 2, 6 ), substr( l_val, 1, 1 ) );
    l_line := l_line || to_png_px( l_mapping( 31 ), l_scale ); -- centre guard
    l_line := l_line || add_bars( substr( l_val, 8, 6 ) );
    l_line := l_line || to_png_px( l_mapping( 30 ), l_scale ); -- right guard
    l_line := l_line || to_png_px( tp_bar_row( -7 ), l_scale ); -- right quiet zone
    --
    l_height := 69 * l_scale;
    l_width := ( length( l_line ) - 2 ) / 2;
    dbms_lob.createtemporary( l_dat, true, dbms_lob.call );
    for i in 1 .. l_height
    loop
      dbms_lob.writeappend( l_dat, l_width + 1, l_line );
    end loop;
    --
    l_fnt := number_font;
    l_line :=    '00' -- filter type None
              || to_png_px( tp_bar_row( -11 ), l_scale ) -- left quiet zone
              || to_png_px( l_mapping( 30 ), l_scale ) -- left guard
              || to_png_px( tp_bar_row( -7 * 6 ), l_scale ) -- 6 numbers
              || to_png_px( l_mapping( 31 ), l_scale ) -- centre guard
              || to_png_px( tp_bar_row( -7 * 6 ), l_scale ) -- 6 numbers
              || to_png_px( l_mapping( 30 ), l_scale ) -- right guard
              || to_png_px( tp_bar_row( -7 ), l_scale ); -- right quiet zone
    dbms_lob.writeappend( l_dat, l_width + 1, l_line );
    for j in 0 .. 8
    loop
      l_line := '00'; -- filter type None
      l_line := l_line || to_png_px( tp_bar_row( -2 ), l_scale ); -- left quiet zone
      l_line := l_line || font_px( l_fnt, 9, 7, substr( l_val, 1, 1 ), j, l_scale );
      l_line := l_line || to_png_px( tp_bar_row( -2 ), l_scale ); -- left quiet zone
      l_line := l_line || to_png_px( case when j < 4 then l_mapping( 30 ) else tp_bar_row( -3 ) end, l_scale ); -- left guard
      for c in 2 .. 7
      loop
        l_line := l_line || font_px( l_fnt, 9, 7, substr( l_val, c, 1 ), j, l_scale );
      end loop;
      l_line := l_line || to_png_px( case when j < 4 then l_mapping( 31 ) else tp_bar_row( -5 ) end, l_scale ); -- centre guard
      for c in 8 .. 13
      loop
        l_line := l_line || font_px( l_fnt, 9, 7, substr( l_val, c, 1 ), j, l_scale );
      end loop;
      l_line := l_line || to_png_px( case when j < 4 then l_mapping( 30 ) else tp_bar_row( -3 ) end, l_scale ); -- right guard
      l_line := l_line || font_px( l_fnt, 9, 7, 11, j, l_scale ); -- >
      for s in 1 .. l_scale
      loop
        dbms_lob.writeappend( l_dat, l_width + 1, l_line );
        l_height := l_height + 1;
      end loop;
    end loop;
    --
    return generate_png( l_dat, l_width, l_height + 1, p_parm );
  end ean13;
  --
  function png_matrix( p_matrix in out nocopy tp_matrix, p_parm varchar2 )
  return raw
  is
    l_dat blob;
    l_line raw(32767);
    l_tmp varchar2(32767);
    l_hsz pls_integer;
    l_vsz pls_integer;
    l_scale pls_integer := 4;
  begin
    --
    l_scale := nvl( check_int( p_parm, 'scale', 10 ), l_scale );
    --
    dbms_lob.createtemporary( l_dat, true, dbms_lob.call );
    l_hsz := p_matrix.count;
    l_vsz := p_matrix(1).count;
    for r in 0 .. l_vsz - 1
    loop
      l_tmp := '00';
      l_line := null;
      for c in 0 .. l_hsz - 1
      loop
        l_tmp := l_tmp || case when p_matrix( c )( r ) > 0
                            then rpad( '00', l_scale * 2, '00' )
                            else rpad( '01', l_scale * 2, '01' )
                          end;
      end loop;
      for j in 1 .. l_scale
      loop
        l_line := utl_raw.concat( l_line, l_tmp );
      end loop;
      dbms_lob.writeappend( l_dat, utl_raw.length( l_line ), l_line );
    end loop;
    p_matrix.delete;
    return generate_png( l_dat, l_hsz * l_scale, l_vsz * l_scale, p_parm );
  end;
  --
  function qrcode( p_val varchar2 character set any_cs, p_parm varchar2 )
  return raw
  is
    l_matrix tp_matrix;
  begin
    gen_qrcode_matrix( p_val, p_parm, l_matrix );
    return png_matrix( l_matrix, p_parm );
  end;
  --
  function aztec( p_val varchar2 character set any_cs, p_parm varchar2 )
  return raw
  is
    l_matrix tp_matrix;
  begin
    gen_aztec_matrix( p_val, p_parm, l_matrix );
    return png_matrix( l_matrix, p_parm );
  end;
  --
  function datamatrix( p_val varchar2 character set any_cs, p_parm varchar2 )
  return raw
  is
    l_matrix tp_matrix;
  begin
    gen_datamatrix_matrix( p_val, p_parm, l_matrix );
    return png_matrix( l_matrix, p_parm );
  end;
  --
  function barcode( p_val varchar2 character set any_cs, p_type varchar2, p_parm varchar2 := null )
  return raw
  is
    t_val varchar2(32767);
    t_tmp pls_integer;
  begin
    begin
      if p_val is not null
      then
        if upper( p_type ) like 'QR%'
        then
          return qrcode( p_val, p_parm );
        elsif upper( p_type ) like 'AZ%'
        then
          return aztec( p_val, p_parm );
        elsif p_type like '%128%'
        then
          return code128( p_val, p_parm );
        elsif upper( p_type ) like '%EAN%13%'
        then
          return ean13( p_val, p_parm );
        elsif upper( p_type ) like '%EAN%8%'
        then
          return ean8( p_val, p_parm );
        elsif p_type like '%39%'
        then
          return code39( p_val, p_parm );
        elsif upper( p_type ) like '%ITF%'
           or upper( p_type ) like 'GTIN%14%'
           or upper( p_type ) like 'INTERLEAVED%'
        then
          return itf( p_val, p_parm );
        elsif upper( p_type ) like '%DATA%'
           or upper( p_type ) like '%MATRIX%'
        then
          return datamatrix( p_val, p_parm );
        end if;
      end if;
    exception
      when others
        then
 dbms_output.put_line( dbms_utility.format_error_stack );dbms_output.put_line( dbms_utility.format_error_backtrace );
          raise;
    end;
    return '89504E470D0A1A0A0000000D49484452000000010000000108060000001F15C4890000000D4944415478DA63F8FF9FA11E00077D027EFDBCECEE0000000049454E44AE426082';
  end;
  --
  procedure download_barcode( p_val varchar2 character set any_cs, p_type varchar2, p_parm varchar2 := null )
  is
    t_img raw(32767);
  begin
    t_img := barcode( p_val, p_type, p_parm );
    htp.init;
    owa_util.mime_header( 'image/png', false );
    htp.p( 'Content-length: ' || utl_raw.length( t_img ) );
    htp.p( 'Content-Disposition: : inline;' );
    htp.p( 'Cache-Control: no-store, no-cache, must-revalidate' );
    owa_util.http_header_close;
    wpg_docload.download_file( p_blob => t_img );
  end;
  --
  function datauri_barcode( p_val varchar2 character set any_cs, p_type varchar2, p_parm varchar2 := null )
  return clob
  is
  begin
    return 'data:image/png;base64,' || utl_raw.cast_to_varchar2( utl_encode.base64_encode( barcode( p_val, p_type, p_parm ) ) );
  end;
--
  function svg_to_char( p_val number, p_enclosed boolean := true )
  return varchar2
  is
  begin
    return case when p_enclosed then '"' end ||
           rtrim( to_char( p_val, 'fm9990.9999' ), '.' ) ||
           case when p_enclosed then '"' end;
  end;
  --
  function svg_header( p_svg clob
                     , p_parm varchar2 := null
                     , p_size number := null
                     , p_width number := null
                     , p_height number := null
                     , p_font boolean := false
                     )
  return clob
  is
    l_marge pls_integer;
    l_height number;
    l_width  number;
    l_tmp varchar2(1000);
    l_viewbox varchar2(1000);
    l_svg_id     varchar2(1000);
    l_svg_size   varchar2(1000);
    l_background varchar2(1000);
    l_foreground varchar2(1000);
    l_bg_color   varchar2(100);
    l_fg_color   varchar2(100);
    l_font       varchar2(4000);
  begin
    l_marge := nvl( check_int( p_parm, 'marge', 20 ), 2 );
    --
    if p_size is not null
    then
      l_width := p_size;
      l_height := p_size;
    else
      l_width := p_width;
      l_height := p_height;
    end if;
    --
    begin
      l_tmp := xjv( p_parm, 'width' );
      if l_tmp is not null
      then
        l_svg_size := ' width=' || svg_to_char( l_tmp );
      end if;
      l_tmp := xjv( p_parm, 'height' );
      if l_tmp is not null
      then
        l_svg_size := l_svg_size || ' height=' || svg_to_char( l_tmp );
      end if;
    exception when others then l_svg_size := null;
    end;
    begin
      l_tmp := xjv( p_parm, 'size' );
      if l_tmp is not null
      then
        l_svg_size := ' width=' || svg_to_char( l_tmp )
                   || ' height=' || svg_to_char( l_tmp );
      end if;
    exception when others then l_svg_size := null;
    end;
    --
    if p_font
    then
      l_font := ' text-anchor="middle" dominant-baseline="central" ' ||
                'font-family="' || jv( p_parm, 'font-family' ) ||
                ',Courier New,Lucida Console,Nimbus Mono L,Free Mono,Liberation Mono,DejaVu Mono,Monaco" ';
    end if;
    --
    begin
      l_bg_color := '#fff';
      l_bg_color := coalesce( xjv( p_parm, 'light' )
                            , xjv( p_parm, 'light_color' )
                            , l_bg_color
                            );
    exception when others then null;
    end;
    l_background := '<rect fill="' || l_bg_color || '"' ||
                    ' width=' || svg_to_char( l_width ) ||
                    ' height=' || svg_to_char( l_height ) ||
                    '/>';
    --
    begin
      l_fg_color := '#000';
      l_fg_color := coalesce( xjv( p_parm, 'dark' )
                            , xjv( p_parm, 'dark_color' )
                            , l_fg_color
                            );
    exception when others then null;
    end;
    l_foreground := '<g fill="' || l_fg_color || '" ' || l_font ||
                   'stroke="' || l_fg_color || '" stroke-width="0.05">';
    --
    l_viewbox := ' viewBox="' ||
                 svg_to_char( - l_marge, false ) || ' ' ||
                 svg_to_char( - l_marge, false ) || ' ' ||
                 svg_to_char( l_width + 2 * l_marge, false ) || ' ' ||
                 svg_to_char( l_height + 2 * l_marge, false ) || '"';
    --
    begin
      l_tmp := xjv( p_parm, 'id' );
      if l_tmp is not null
      then
        l_svg_id := ' id="' || l_tmp || '"';
      end if;
    exception when others then null;
    end;
    --
    return ( '<svg xmlns="http://www.w3.org/2000/svg"' ||
             l_svg_size || l_svg_id || l_viewbox || '>' ||
             l_background || l_foreground
           ) || p_svg || '</g></svg>';
  end;
  --
  function svg_rect( x number, h number, w number, y number := 0 )
  return varchar2
  is
  begin
    return '<rect'
         || ' x=' || svg_to_char( x )
         || ' width=' || svg_to_char( w )
         || ' height=' || svg_to_char( h )
         || ' y=' || svg_to_char( y ) || '/>';
  end;
  --
  procedure ean_bar( p_svg in out varchar2
                     , p_x in out number
                     , p_bar tp_bar_row
                     , p_h number
                     , p_y number := 0
                     )
  is
    l_w number;
  begin
    for i in p_bar.first .. p_bar.last
    loop
      l_w := p_bar( i );
      if l_w > 0
      then
        p_svg := p_svg || svg_rect( p_x, p_h, l_w, p_y );
      end if;
      p_x := p_x + abs( l_w );
    end loop;
  end;
  --
  procedure greater_less_human( p_svg in out varchar2
                              , p_x in out number
                              , p_y number
                              , p_greater boolean
                         )
  is
  begin
    p_x := p_x + case when p_greater then 0 else 7 end;
    p_svg := p_svg || '<g transform="translate('
                   || svg_to_char( p_x, false ) || ','
                   || svg_to_char( p_y, false ) || ') scale('
                   || case when not p_greater then '-' end
                   || '1,1)"><path d="'
                   || 'M0.6 0v-1l5.7-3.5l-5.7-3.5v-1l6.4 4 v1z"/></g>';
    p_x := p_x + case when p_greater then 7 else 0 end;
  end;
  --
  function check_ean_val_add_checknr( p_val varchar2, p_len pls_integer )
  return varchar2
  is
    l_val varchar2(100);
  begin
    if    ltrim( p_val ) is null
       or ltrim( p_val, ' 0123456789' ) is not null
       or length( trim( p_val ) ) > p_len
       or trim( p_val ) <> replace( p_val, ' ' )
    then
      return null;
    end if;
    l_val := trim( p_val );
    if length( l_val ) = p_len
    then
      return l_val;
    end if;
    l_val := lpad( l_val, p_len - 1, '0' );
    return l_val || upc_checksum( l_val );
  end;
  --
  procedure svg_centered_text( p_svg in out clob
                             , p_val        varchar2
                             , p_y          number
                             , p_width      number
                             , p_x          number := 0
                             , p_height     number := 15
                             , p_scale      number := 80
                             )
  is
  begin
    p_svg := p_svg || '<svg ' ||
       ' x=' || svg_to_char( p_x ) ||
       ' y=' || svg_to_char( p_y ) ||
       ' height=' || svg_to_char( p_height ) ||
       ' width=' || svg_to_char( p_width ) || '>' ||
       '<text font-size="' || svg_to_char( p_scale, false ) || '%" x="50%" y="50%">' ||
       p_val || '</text></svg>';
  end;
  --
  procedure ean2_svg( p_svg in out varchar2
                    , p_val        varchar2
                    , p_x   in out number
                    , p_h          number
                    , p_h2         number
                    , p_mapping    tp_mapping
                    )
  is
    l_v pls_integer;
  begin
    ean_bar( p_svg, p_x, p_mapping( 33 ), p_h, p_h2 ); -- add-on guard bar
    l_v := mod( to_number( p_val ), 4 );
    svg_centered_text( p_svg, substr( p_val, 1, 1 ), 0, 7, p_x, p_h2, 75 );
    ean_bar( p_svg, p_x, p_mapping( case when l_v in ( 0, 1 ) then 0 else 10 end + substr( p_val, 1, 1 ) ), p_h, p_h2 );
    ean_bar( p_svg, p_x, p_mapping( 34 ), p_h, p_h2 ); -- add-on delineator
    svg_centered_text( p_svg, substr( p_val, 2, 1 ), 0, 7, p_x, p_h2, 75 );
    ean_bar( p_svg, p_x, p_mapping( case when l_v in ( 0, 2 ) then 0 else 10 end + substr( p_val, 2, 1 ) ), p_h, p_h2 );
    greater_less_human( p_svg, p_x, p_h2, true );
  end ean2_svg;
  --
  procedure ean5_svg( p_svg in out varchar2
                    , p_val        varchar2
                    , p_x   in out number
                    , p_h          number
                    , p_h2         number
                    , p_mapping    tp_mapping
                    )
  is
    l_code pls_integer;
    l_v pls_integer := 0;
  begin
    ean_bar( p_svg, p_x, p_mapping( 33 ), p_h, p_h2 ); -- add-on guard bar
    for i in 1 .. 5
    loop
      l_v := l_v + to_number( substr( p_val, i, 1 ) )
                 * case when mod( i, 2 ) = 1 then 3 else 9 end;
    end loop;
    l_v := mod( l_v, 10 );
    for i in 1 .. 5
    loop
      l_code := substr( '11000101001001010001011000011000011010100100100101'
                      , l_v * 5 + i
                      , 1
                      ) * 10;
      svg_centered_text( p_svg, substr( p_val, i, 1 ), 0, 7, p_x, p_h2, 75 );
      ean_bar( p_svg, p_x, p_mapping( l_code + substr( p_val, i, 1 ) ), p_h, p_h2 );
      if i < 5
      then
        ean_bar( p_svg, p_x, p_mapping( 34 ), p_h, p_h2 ); -- add-on delineator
      end if;
    end loop;
    greater_less_human( p_svg, p_x, p_h2, true );
  end ean5_svg;
  --
  function ean13_svg( p_val varchar2, p_parm varchar2 )
  return varchar2
  is
    l_val varchar2(100);
    l_svg varchar2(32767);
    l_x number := 0;
    l_h number;
    l_h1 number := 69.2424;
    l_h2 number := 9.3333;
    l_ld pls_integer;
    l_addon varchar2(100);
    l_mapping tp_mapping;
  begin
    l_val := check_ean_val_add_checknr( p_val, 13 );
    init_ean_digits( l_mapping );
    svg_centered_text( l_svg, substr( l_val, 1, 1 ), l_h1, 7, l_x, l_h2, 75 );
    l_x := l_x + 7;
    ean_bar( l_svg, l_x, l_mapping( 30 ), l_h1 + 0.5 * l_h2 ); -- left guard bar
    --
    l_ld := substr( l_val, 1, 1 );
    svg_centered_text( l_svg, substr( l_val, 2, 1 ), l_h1, 7, l_x, l_h2, 75 );
    ean_bar( l_svg, l_x, l_mapping( 0 + substr( l_val, 2, 1 ) ), l_h1 );
    svg_centered_text( l_svg, substr( l_val, 3, 1 ), l_h1, 7, l_x, l_h2, 75 );
    ean_bar( l_svg, l_x, l_mapping( case when l_ld < 4 then 0 else 10 end + substr( l_val, 3, 1 ) ), l_h1 );
    svg_centered_text( l_svg, substr( l_val, 4, 1 ), l_h1, 7, l_x, l_h2, 75 );
    ean_bar( l_svg, l_x, l_mapping( case when l_ld  in ( 0, 4, 7, 8 ) then 0 else 10 end + substr( l_val, 4, 1 ) ), l_h1 );
    svg_centered_text( l_svg, substr( l_val, 5, 1 ), l_h1, 7, l_x, l_h2, 75 );
    ean_bar( l_svg, l_x, l_mapping( case when l_ld  in ( 0, 1, 4, 5, 9 ) then 0 else 10 end + substr( l_val, 5, 1 ) ), l_h1 );
    svg_centered_text( l_svg, substr( l_val, 6, 1 ), l_h1, 7, l_x, l_h2, 75 );
    ean_bar( l_svg, l_x, l_mapping( case when l_ld  in (  0, 2, 5, 6, 7 ) then 0 else 10 end + substr( l_val, 6, 1 ) ), l_h1 );
    svg_centered_text( l_svg, substr( l_val, 7, 1 ), l_h1, 7, l_x, l_h2, 75 );
    ean_bar( l_svg, l_x, l_mapping( case when l_ld  in ( 0, 3, 6, 8, 9 ) then 0 else 10 end + substr( l_val, 7, 1 ) ), l_h1 );
    --
    ean_bar( l_svg, l_x, l_mapping( 31 ), l_h1 + 0.5 * l_h2 ); -- centre guard bar
    --
    for i in 8 .. 13
    loop
      svg_centered_text( l_svg, substr( l_val, i, 1 ), l_h1, 7, l_x, l_h2, 75 );
      ean_bar( l_svg, l_x, l_mapping( 20 + substr( l_val, i, 1 ) ), l_h1 );
    end loop;
    ean_bar( l_svg, l_x, l_mapping( 30 ), l_h1 + 0.5 * l_h2 ); -- right guard bar
    --
    if p_parm is not null
    then
      l_addon := coalesce( jv( p_parm, 'addon' )
                         , jv( p_parm, 'ean2' )
                         , jv( p_parm, 'ean5' )
                         , substr( trim( p_parm ), 1, 10 )
                         );
      if    ltrim( l_addon ) is null
         or ltrim( l_addon, ' 0123456789' ) is not null
         or length( trim( l_addon ) ) > 5
      then
        l_addon := null;
      end if;
    end if;
    if l_addon is null
    then
      greater_less_human( l_svg, l_x, l_h1 + l_h2, true );
    else
      l_x := l_x + 7;
      l_h := l_h1 - 0.5 * l_h2;
      l_addon := trim( l_addon );
      if length( l_addon ) > 2 or jv( p_parm, 'ean5' ) is not null
      then
        ean5_svg( l_svg, lpad( l_addon, 5, '0' ), l_x, l_h, l_h2, l_mapping );
      else
        ean2_svg( l_svg, lpad( l_addon, 2, '0' ), l_x, l_h, l_h2, l_mapping );
      end if;
    end if;
    l_mapping.delete;
    return svg_header( l_svg, p_parm, p_width => l_x, p_height => l_h1 + l_h2, p_font => true );
  end;
  --
  function ean8_svg( p_val varchar2, p_parm varchar2 )
  return varchar2
  is
    l_val varchar2(100);
    l_svg varchar2(32767);
    l_x number := 0;
    l_h1 number := 55.2424;
    l_h2 number := 9.3333;
    l_mapping tp_mapping;
  begin
    l_val := check_ean_val_add_checknr( p_val, 8 );
    init_ean_digits( l_mapping );
    greater_less_human( l_svg, l_x, l_h1 + l_h2, false );
    ean_bar( l_svg, l_x, l_mapping( 30 ), l_h1 + 0.5 * l_h2 ); -- left guard bar
    for i in 1 .. 4
    loop
      svg_centered_text( l_svg, substr( l_val, i, 1 ), l_h1, 7, l_x, l_h2, 75 );
      ean_bar( l_svg, l_x, l_mapping( substr( l_val, i, 1 ) ), l_h1 );
    end loop;
    ean_bar( l_svg, l_x, l_mapping( 31 ), l_h1 + 0.5 * l_h2 ); -- centre guard bar
    for i in 5 .. 8
    loop
      svg_centered_text( l_svg, substr( l_val, i, 1 ), l_h1, 7, l_x, l_h2, 75 );
      ean_bar( l_svg, l_x, l_mapping( 20 + substr( l_val, i, 1 ) ), l_h1 );
    end loop;
    ean_bar( l_svg, l_x, l_mapping( 30 ), l_h1 + 0.5 * l_h2 ); -- right guard bar
    greater_less_human( l_svg, l_x, l_h1 + l_h2, true );
    l_mapping.delete;
    return svg_header( l_svg, p_parm, p_width => l_x, p_height => l_h1 + l_h2, p_font => true );
  end ean8_svg;
  --
  function svg_matrix( p_matrix in out tp_matrix, p_parm varchar2 )
  return clob
  is
    l_svg clob;
    l_hsz pls_integer;
    l_vsz pls_integer;
  begin
    l_hsz := p_matrix.count;
    l_vsz := p_matrix(1).count;
    for c in 0 .. l_hsz - 1
    loop
      for r in 0 .. l_vsz - 1
      loop
        if p_matrix( c )( r ) > 0
        then
           l_svg := l_svg || svg_rect( c, 1, 1, r );
        end if;
      end loop;
    end loop;
    p_matrix.delete;
    --
    return svg_header( l_svg, p_parm, p_width => l_hsz, p_height => l_vsz );
  end;
  --
  function svg_bar( p_row in out nocopy tp_bar_row
                  , p_parm varchar2
                  , p_height number
                  , p_human varchar2
                  )
  return clob
  is
    l_svg clob;
    c_human_sep constant number := 2;
    c_bearer    constant number := 4;
    l_width   number;
    l_height  number;
    l_start_y number := 0;
  begin
    if nvl( p_height, 0 ) <= 0
    then
      return null;
    end if;
    --
    l_width := 0;
    l_height := p_height;
    if check_pos( p_parm, 'bearer' )
    then
      l_start_y := c_bearer;
    end if;
    for c in 1 .. p_row.count
    loop
      if p_row( c ) > 0
      then
        l_svg := l_svg || svg_rect( l_width, l_height, p_row( c ), l_start_y );
      end if;
      l_width := l_width + abs( p_row( c ) );
    end loop;
    p_row.delete;
    --
    if check_pos( p_parm, 'bearer' )
    then
      l_svg := l_svg || svg_rect( 0, c_bearer, l_width, 0 );
      l_svg := l_svg || svg_rect( 0, c_bearer, l_width, l_start_y + l_height );
      l_height := l_height + 2 * c_bearer;
    end if;
    --
    svg_centered_text( l_svg, p_human, l_height + c_human_sep, l_width );
    --
    return svg_header( l_svg, p_parm, p_height => l_height + 15 + 2 * c_human_sep, p_width => l_width, p_font => true );
  end svg_bar;
  --
  function code128_svg( p_val varchar2 character set any_cs, p_parm varchar2 )
  return clob
  is
    l_row tp_bar_row;
    l_height number;
    l_human varchar2(4000) character set p_val%charset;
  begin
    gen_code128( p_val, p_parm, l_row, l_height, l_human );
    return svg_bar( l_row, p_parm, l_height, l_human );
  end code128_svg;
  --
  function code39_svg( p_val varchar2, p_parm varchar2 )
  return clob
  is
    l_row tp_bar_row;
    l_height number;
    l_human varchar2(4000);
  begin
    gen_code39( p_val, p_parm, l_row, l_height, l_human );
    return svg_bar( l_row, p_parm, l_height, l_human );
  end code39_svg;
  --
  function itf_svg( p_val varchar2, p_parm varchar2 )
  return clob
  is
    l_row tp_bar_row;
    l_height number;
    l_human varchar2(4000);
  begin
    gen_itf( p_val, p_parm, l_row, l_height, l_human );
    return svg_bar( l_row, p_parm, l_height, l_human );
  end itf_svg;
  --
  function qrcode_svg( p_val varchar2 character set any_cs
                     , p_parm varchar2
                     )
  return clob
  is
    l_matrix tp_matrix;
  begin
    gen_qrcode_matrix( p_val, p_parm, l_matrix );
    return svg_matrix( l_matrix, p_parm );
  end;
  --
  function aztec_svg( p_val varchar2 character set any_cs
                    , p_parm varchar2
                    )
  return clob
  is
    l_matrix tp_matrix;
  begin
    gen_aztec_matrix( p_val, p_parm, l_matrix );
    return svg_matrix( l_matrix, p_parm );
  end;
  --
  function datamatrix_svg( p_val varchar2 character set any_cs
                         , p_parm varchar2
                         )
  return clob
  is
    l_matrix tp_matrix;
  begin
    gen_datamatrix_matrix( p_val, p_parm, l_matrix );
    return svg_matrix( l_matrix, p_parm );
  end;
  --
  function barcode_svg( p_val varchar2 character set any_cs
                      , p_type varchar2
                      , p_parm varchar2 := null
                      )
  return clob
  is
    l_empty_svg varchar2(100) := '<svg xmlns="http://www.w3.org/2000/svg"/>';
  begin
    if p_val is not null
    then
      if upper( p_type ) like 'QR%'
      then
        return qrcode_svg( p_val, p_parm );
      elsif upper( p_type ) like 'AZ%'
      then
        return aztec_svg( p_val, p_parm );
      elsif p_type like '%128%'
      then
        return code128_svg( p_val, p_parm );
      elsif upper( p_type ) like '%EAN%13%'
      then
        return ean13_svg( p_val, p_parm );
      elsif upper( p_type ) like '%EAN%8%'
      then
        return ean8_svg( p_val, p_parm );
      elsif upper( p_type ) like '%39%'
      then
        return code39_svg( p_val, p_parm );
      elsif upper( p_type ) like '%ITF%'
         or upper( p_type ) like 'GTIN%14%'
         or upper( p_type ) like 'INTERLEAVED%'
      then
        return itf_svg( p_val, p_parm );
      elsif upper( p_type ) like '%DATA%'
         or upper( p_type ) like '%MATRIX%'
      then
        return datamatrix_svg( p_val, p_parm );
      end if;
    end if;
    return l_empty_svg;
  end;
  --
  function datauri_barcode_svg( p_val varchar2 character set any_cs
                              , p_type varchar2
                              , p_parm varchar2 := null
                              )
  return clob
  is
  begin
    return 'data:image/svg+xml;base64,' || utl_raw.cast_to_varchar2( utl_encode.base64_encode( utl_raw.cast_to_raw( barcode_svg( p_val, p_type, p_parm ) ) ) );
  end;
  --
end;
