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
**   Date: 2018-06-25 kevin@kevindurette.com
**     Added function barcode_blob to return BLOB instead of RAW
******************************************************************************
******************************************************************************
Copyright (C) 2016 by Anton Scheffer

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
  type tp_ean_bars is table of raw(7) index by pls_integer;
  type tp_bits is table of pls_integer index by pls_integer;
--
  procedure save_raw
    ( p_file raw
    , p_dir varchar2 := 'HOME'
    , p_filename varchar2 := 'test.png'
    )
  is
    t_fh utl_file.file_type;
    t_len pls_integer := 32767;
  begin
    t_fh := utl_file.fopen( p_dir, p_filename, 'wb' );
    utl_file.put_raw( t_fh, p_file );
    utl_file.fclose( t_fh );
  end;
--
  function generate_png( p_dat blob, p_width pls_integer, p_height pls_integer )
  return raw
  is
    t_ihdr raw(25);
    t_idat raw(32767);
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
                            , '0800000000'  -- Bit depth 8
                                            -- Colour type 0
                                            -- Compression method 0
                                            -- Filter method 0
                                            -- Interlace method 0
                            );
    t_idat := utl_raw.concat( '49444154' -- IDAT
                            , method0_compress( p_dat )
                            );
    return utl_raw.concat( '89504E470D0A1A0A' -- signature
                         , '0000000D'
                         , t_ihdr
                         , crc32( t_ihdr )
                         , to_char( utl_raw.length( t_idat ) - 4, 'fm0XXXXXXX' )
                         , t_idat
                         , crc32( t_idat )
                         , '0000000049454E44AE426082' -- IEND
                         );
  end;
--
  function bitxor( x number, y number )
  return number
  is
  begin
    return x + y - 2 * bitand( x, y );
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
--      t_g(0) := bitxor( 0, t_exp( t_log( t_g( 0 ) ) + t_log( 1 ) ) ); 
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
  function reed_solomon
    ( p_data raw
    , p_primitive pls_integer := 285
    , p_size pls_integer := 256
    , p_degree pls_integer := 16
    , p_b pls_integer := 0
    )
  return raw
  is
    t_bits tp_bits;
    t_rv raw(3000);
  begin
    for i in 1 .. utl_raw.length( p_data )
    loop
      t_bits( i - 1 ) := to_number( utl_raw.substr( p_data, i, 1 ), 'xx' );
    end loop;
    t_bits := reed_solomon( t_bits, p_primitive, p_size, p_degree, p_b );
    for i in t_bits.first .. t_bits.last
    loop
      t_rv := utl_raw.concat( t_rv, to_char( t_bits( i ), 'fm0X' ) );
    end loop;
    return t_rv;    
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
                                 , case when bitand( t_line, power( 2, j ) ) = 0 then 'FF' else '00' end
                                 );
        end loop;
      end loop;
    end loop;
    return t_fnt;
  end;
--
-- a 8x14 font, containing codepage 1252, which happens to be a superset of ISO-8859-1
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
                                 , case when bitand( t_line, power( 2, j ) ) = 0 then 'FF' else '00' end
                                 );
        end loop;
      end loop;
    end loop;
    return t_fnt;
  end;
--
  procedure get_ean_bars( p_bars in out tp_ean_bars )
  is
    t_def raw(70) := utl_raw.concat( '000000FFFF00FF0000FFFF0000FF0000FF00'
                                   , '00FFFF00FFFFFFFF00FF00FF000000FFFF00'
                                   , 'FFFF000000FF00FF00FFFFFFFF00FFFFFF00'
                                   , 'FFFF00FFFF00FFFFFF000000FF00FFFF'
                                   );
  begin
    for i in 0 .. 9
    loop
      p_bars(i+20) := utl_raw.substr( t_def, i*7 + 1, 7 ); -- R-code
      p_bars(i) := utl_raw.bit_complement( p_bars(i+20) ); -- L-code
      p_bars(i+10) := utl_raw.reverse( p_bars(i+20) );     -- G-code
    end loop;
  end;
--
  function ean2( p_val varchar2 )
  return raw
  is
    t_dat raw(3999);
    t_line raw(200);
    t_height pls_integer := 40;
    t_bar tp_ean_bars;
    t_fnt raw(1000);
    t_idx1 pls_integer;
    t_idx2 pls_integer;
    t_space pls_integer := 1;
  begin
--
    get_ean_bars( t_bar );
    t_fnt := number_font;
--
    t_idx1 := ascii( p_val ) - 48;
    t_idx2 := ascii( substr( p_val, 2 ) ) - 48;
--
    for j in 0 .. 8
    loop
      t_dat := utl_raw.concat( t_dat
                             , '00' -- filter type None
                             , utl_raw.copies( 'FF', 7 ) -- left quiet zone
                             , 'FFFFFFFFFF' -- start
                             , utl_raw.substr( t_fnt, ( t_idx1 * 9 + j ) * 7 + 1, 7 )
                             , 'FFFF'       -- separator
                             , utl_raw.substr( t_fnt, ( t_idx2 * 9 + j ) * 7 + 1, 7 )
                             , utl_raw.copies( 'FF', 7 ) -- right quiet zone
                             );
    end loop;
--
    for i in 1 .. t_space
    loop
      t_dat := utl_raw.concat( t_dat
                             , '00' -- filter type None
                             , utl_raw.copies( 'FF', 7 ) -- left quiet zone
                             , 'FFFFFFFFFF' -- start
                             , utl_raw.copies( 'FF', 7 ) -- first char
                             , 'FFFF'       -- separator
                             , utl_raw.copies( 'FF', 7 ) -- second char
                             , utl_raw.copies( 'FF', 7 ) -- right quiet zone
                             );
    end loop;
--
    case mod( to_number( p_val ), 4 )
      when 0 then
        null;
      when 1 then
        t_idx2 := t_idx2 + 10;
      when 2 then
        t_idx1 := t_idx1 + 10;
      when 3 then
        t_idx1 := t_idx1 + 10;
        t_idx2 := t_idx2 + 10;
    end case;
--
    t_line := utl_raw.concat( '00' -- filter type None
                            , utl_raw.copies( 'FF', 7 ) -- left quiet zone
                            , 'FF00FF0000' -- start
                            , t_bar( t_idx1 )
                            , 'FF00'       -- separator
                            , t_bar( t_idx2 )
                            , utl_raw.copies( 'FF', 7 ) -- right quiet zone
                            );
--
    for i in 1 .. t_height
    loop
      t_dat := utl_raw.concat( t_dat, t_line );
    end loop;
--
    return generate_png( t_dat, utl_raw.length( t_line ) - 1, t_height + 9 + t_space );
  end;
--
  function ean5( p_val varchar2 )
  return raw
  is
    t_dat raw(3999);
    t_line raw(200);
    t_height pls_integer := 40;
    t_bar tp_ean_bars;
    t_fnt raw(1000);
    type tp_idx is table of pls_integer index by pls_integer;
    t_idx tp_idx;
    t_check pls_integer := 0;
    t_space pls_integer := 1;
  begin
--
    get_ean_bars( t_bar );
    t_fnt := number_font;
--
    for i in 1 .. 5
    loop
      t_idx(i) := ascii( substr( p_val, i ) ) - 48;
      t_check := t_check
               + to_number( substr( p_val, i, 1 ) )
               * case when mod( i, 2 ) = 1 then 3 else 9 end;
    end loop;
    t_check := mod( t_check, 10 );
--
    for j in 0 .. 8
    loop
      t_dat := utl_raw.concat( t_dat
                             , '00' -- filter type None
                             , utl_raw.copies( 'FF', 7 ) -- left quiet zone
                             , 'FFFFFFFFFF' -- start
                             );
      for i in 1 .. 5
      loop
        t_dat := utl_raw.concat( t_dat
                               , utl_raw.substr( t_fnt, ( t_idx(i) * 9 + j ) * 7 + 1, 7 )
                               , case
                                   when i < 5 then 'FFFF'         -- separator
                                   else utl_raw.copies( 'FF', 7 ) -- right quiet zone
                                 end
                               );
      end loop;
    end loop;
--
    for i in 1 .. t_space
    loop
      t_dat := utl_raw.concat( t_dat
                             , '00' -- filter type None
                             , utl_raw.copies( 'FF', 7 )     -- left quiet zone
                             , 'FFFFFFFFFF'                  -- start
                             , utl_raw.copies( 'FF', 7 * 5 ) -- 5 chars
                             , utl_raw.copies( 'FF', 2 * 4 ) -- 4 separators
                             , utl_raw.copies( 'FF', 7 )     -- right quiet zone
                             );
    end loop;
--
    t_line := utl_raw.concat( '00' -- filter type None
                            , utl_raw.copies( 'FF', 7 ) -- left quiet zone
                            , 'FF00FF0000' -- start
                            );
    for i in 1 .. 5
    loop
      if substr( 'GGLLLGLGLLGLLGLGLLLGLGGLLLLGGLLLLGGLGLGLLGLLGLLGLG', t_check * 5 + i, 1 ) = 'G'
      then
        t_idx( i ) := t_idx( i ) + 10;
      end if;
      t_line := utl_raw.concat( t_line
                              , t_bar( t_idx(i) )
                              , case
                                  when i < 5 then 'FF00'         -- separator
                                  else utl_raw.copies( 'FF', 7 ) -- right quiet zone
                                end
                            );
    end loop;
--
    for i in 1 .. t_height
    loop
      t_dat := utl_raw.concat( t_dat, t_line );
    end loop;
--
    return generate_png( t_dat, utl_raw.length( t_line ) - 1, t_height + 9 + t_space );
  end;
--
  function ean8( p_val varchar2 )
  return raw
  is
    t_dat raw(32767);
    t_line raw(200);
    t_height pls_integer := 55;
    t_bar tp_ean_bars;
    t_fnt raw(1000);
--
    function add_bars( p_val varchar2, p_bar_index pls_integer )
    return raw
    is
    begin
      return utl_raw.concat( t_bar( p_bar_index + ascii( substr( p_val, 1 ) ) - 48 )
                           , t_bar( p_bar_index + ascii( substr( p_val, 2 ) ) - 48 )
                           , t_bar( p_bar_index + ascii( substr( p_val, 3 ) ) - 48 )
                           , t_bar( p_bar_index + ascii( substr( p_val, 4 ) ) - 48 )
                           );
    end;
  begin
--
    get_ean_bars( t_bar );
    t_fnt := number_font;
--
    t_line := '00'; -- filter type None
    t_line := utl_raw.concat( t_line, utl_raw.copies( 'FF', 7 ) ); -- left quiet zone
--
    t_line := utl_raw.concat( t_line, '00FF00' ); -- Normal Guard Bar Pattern
    t_line := utl_raw.concat( t_line, add_bars( substr( p_val, 1, 4 ), 0 ) );
    t_line := utl_raw.concat( t_line, 'FF00FF00FF' ); -- Centre Guard Bar Pattern
    t_line := utl_raw.concat( t_line, add_bars( substr( p_val, 5, 4 ), 20 ) );
    t_line := utl_raw.concat( t_line, '00FF00' ); -- Normal Guard Bar Pattern
    t_line := utl_raw.concat( t_line, utl_raw.copies( 'FF', 7 ) ); -- right quiet zone
--
    for i in 1 .. t_height
    loop
      t_dat := utl_raw.concat( t_dat, t_line );
    end loop;
--
    t_dat := utl_raw.concat( t_dat, '00' ); -- filter type None
    t_dat := utl_raw.concat( t_dat, utl_raw.copies( 'FF', 7 ) ); -- left quiet zone
    t_dat := utl_raw.concat( t_dat, '00FF00' ); -- Normal Guard Bar Pattern
    t_dat := utl_raw.concat( t_dat, utl_raw.copies( 'FF', 7 * 4) );
    t_dat := utl_raw.concat( t_dat, 'FF00FF00FF' ); -- Centre Guard Bar Pattern
    t_dat := utl_raw.concat( t_dat, utl_raw.copies( 'FF', 7 * 4) );
    t_dat := utl_raw.concat( t_dat, '00FF00' ); -- Normal Guard Bar Pattern
    t_dat := utl_raw.concat( t_dat, utl_raw.copies( 'FF', 7 ) ); -- right quiet zone
    for j in 0 .. 8
    loop
      t_dat := utl_raw.concat( t_dat, '00' ); -- filter type None
      t_dat := utl_raw.concat( t_dat, utl_raw.substr( t_fnt, ( 90 + j ) * 7 + 1, 7 ) ); -- left quiet zone
      t_dat := utl_raw.concat( t_dat, case when j < 4 then '00FF00' else 'FFFFFF' end ); -- Normal Guard Bar Pattern
      for c in 1 .. 4
      loop
        t_dat := utl_raw.concat( t_dat, utl_raw.substr( t_fnt, ( ( ascii( substr( p_val, c ) ) - 48 ) * 9 + j ) * 7 + 1, 7 ) );
      end loop;
      t_dat := utl_raw.concat( t_dat, case when j < 4 then 'FF00FF00FF' else 'FFFFFFFFFF' end ); -- Centre Guard Bar Pattern
      for c in 5 .. 8
      loop
        t_dat := utl_raw.concat( t_dat, utl_raw.substr( t_fnt, ( ( ascii( substr( p_val, c ) ) - 48 ) * 9 + j ) * 7 + 1, 7 ) );
      end loop;
      t_dat := utl_raw.concat( t_dat, case when j < 4 then '00FF00' else 'FFFFFF' end ); -- Normal Guard Bar Pattern
      t_dat := utl_raw.concat( t_dat, utl_raw.substr( t_fnt, ( 99 + j ) * 7 + 1, 7 ) ); -- right quiet zone
    end loop;
--
    return generate_png( t_dat, 81, t_height + 10 );
  end;
--
  function ean13( p_val varchar2 )
  return raw
  is
    t_dat raw(32767);
    t_line raw(200);
    t_height pls_integer := 69;
    type tp_bar is table of raw(7) index by pls_integer;
    t_bar tp_ean_bars;
    t_fnt raw(1000);
--
    function add_bars( p_val varchar2, p_check varchar2 := null )
    return raw
    is
      type tp_bar_index is table of pls_integer index by pls_integer;
      t_bar_index tp_bar_index;
      t_check_number pls_integer;
    begin
      if p_check is null
      then
        for i in 1 .. 6
        loop
          t_bar_index(i) := 20;
        end loop;
      else
        t_check_number := ascii( p_check ) - 48;
        t_bar_index(1) := 0;
        t_bar_index(2) := case when t_check_number < 4 then 0 else 10 end;
        t_bar_index(3) := case when t_check_number in ( 0, 4, 7, 8 ) then 0 else 10 end;
        t_bar_index(4) := case when t_check_number in ( 0, 1, 4, 5, 9 ) then 0 else 10 end;
        t_bar_index(5) := case when t_check_number in ( 0, 2, 5, 6, 7 ) then 0 else 10 end;
        t_bar_index(6) := case when t_check_number in ( 0, 3, 6, 8, 9 ) then 0 else 10 end;
      end if;
      return utl_raw.concat( t_bar( t_bar_index(1) + ascii( substr( p_val, 1 ) ) - 48 )
                           , t_bar( t_bar_index(2) + ascii( substr( p_val, 2 ) ) - 48 )
                           , t_bar( t_bar_index(3) + ascii( substr( p_val, 3 ) ) - 48 )
                           , t_bar( t_bar_index(4) + ascii( substr( p_val, 4 ) ) - 48 )
                           , t_bar( t_bar_index(5) + ascii( substr( p_val, 5 ) ) - 48 )
                           , t_bar( t_bar_index(6) + ascii( substr( p_val, 6 ) ) - 48 )
                           );
    end;
  begin
--
    get_ean_bars( t_bar );
    t_fnt := number_font;
--
    t_line := '00'; -- filter type None
    t_line := utl_raw.concat( t_line, utl_raw.copies( 'FF', 11 ) ); -- left quiet zone
    t_line := utl_raw.concat( t_line, '00FF00' ); -- Normal Guard Bar Pattern
    t_line := utl_raw.concat( t_line, add_bars( substr( p_val, 2, 6 ), p_val ) );
    t_line := utl_raw.concat( t_line, 'FF00FF00FF' ); -- Centre Guard Bar Pattern
    t_line := utl_raw.concat( t_line, add_bars( substr( p_val, 8, 6 ) ) );
    t_line := utl_raw.concat( t_line, '00FF00' ); -- Normal Guard Bar Pattern
    t_line := utl_raw.concat( t_line, utl_raw.copies( 'FF', 7 ) ); -- right quiet zone
--
    for i in 1 .. t_height
    loop
      t_dat := utl_raw.concat( t_dat, t_line );
    end loop;
--
    t_dat := utl_raw.concat( t_dat, '00' ); -- filter type None
    t_dat := utl_raw.concat( t_dat, utl_raw.copies( 'FF', 11 ) ); -- left quiet zone
    t_dat := utl_raw.concat( t_dat, '00FF00' ); -- Normal Guard Bar Pattern
    t_dat := utl_raw.concat( t_dat, utl_raw.copies( 'FF', 7 * 6) );
    t_dat := utl_raw.concat( t_dat, 'FF00FF00FF' ); -- Centre Guard Bar Pattern
    t_dat := utl_raw.concat( t_dat, utl_raw.copies( 'FF', 7 * 6) );
    t_dat := utl_raw.concat( t_dat, '00FF00' ); -- Normal Guard Bar Pattern
    t_dat := utl_raw.concat( t_dat, utl_raw.copies( 'FF', 7 ) ); -- right quiet zone
    for j in 0 .. 8
    loop
      t_dat := utl_raw.concat( t_dat, '00' ); -- filter type None
      t_dat := utl_raw.concat( t_dat
                             , 'FFFF'
                             , utl_raw.substr( t_fnt, ( ( ascii( p_val ) - 48 ) * 9 + j ) * 7 + 1, 7 )
                             , 'FFFF'
                             ); -- left quiet zone
      t_dat := utl_raw.concat( t_dat, case when j < 4 then '00FF00' else 'FFFFFF' end ); -- Normal Guard Bar Pattern
      for c in 2 .. 7
      loop
        t_dat := utl_raw.concat( t_dat, utl_raw.substr( t_fnt, ( ( ascii( substr( p_val, c ) ) - 48 ) * 9 + j ) * 7 + 1, 7 ) );
      end loop;
      t_dat := utl_raw.concat( t_dat, case when j < 4 then 'FF00FF00FF' else 'FFFFFFFFFF' end ); -- Centre Guard Bar Pattern
      for c in 8 .. 13
      loop
        t_dat := utl_raw.concat( t_dat, utl_raw.substr( t_fnt, ( ( ascii( substr( p_val, c ) ) - 48 ) * 9 + j ) * 7 + 1, 7 ) );
      end loop;
      t_dat := utl_raw.concat( t_dat, case when j < 4 then '00FF00' else 'FFFFFF' end ); -- Normal Guard Bar Pattern
      t_dat := utl_raw.concat( t_dat, utl_raw.substr( t_fnt, ( 99 + j ) * 7 + 1, 7 ) ); -- right quiet zone
    end loop;
--
    return generate_png( t_dat, 113, t_height + 10 );
  end;
--
  function code39( p_val varchar2 )
  return raw
  is
    t_dat blob;
    t_line raw(32767);
    t_width pls_integer;
    t_height pls_integer := 45;
    c_ratio constant pls_integer := 3; -- 2 or 3
    t_start_stop raw(18);
    type tp_bars is table of raw(18) index by pls_integer;
    t_bars tp_bars;
    t_pre pls_integer;
    t_post pls_integer;
    t_fnt raw(32767);
--
    function char_bars( p_char varchar2 )
    return raw
    is
    begin
      return utl_raw.concat( utl_raw.copies( '00', case when substr( p_char, 1, 1 ) = 1 then c_ratio else 1 end )
                           , utl_raw.copies( 'FF', case when substr( p_char, 2, 1 ) = 1 then c_ratio else 1 end )
                           , utl_raw.copies( '00', case when substr( p_char, 3, 1 ) = 1 then c_ratio else 1 end )
                           , utl_raw.copies( 'FF', case when substr( p_char, 4, 1 ) = 1 then c_ratio else 1 end )
                           , utl_raw.copies( '00', case when substr( p_char, 5, 1 ) = 1 then c_ratio else 1 end )
                           , utl_raw.copies( 'FF', case when substr( p_char, 6, 1 ) = 1 then c_ratio else 1 end )
                           , utl_raw.copies( '00', case when substr( p_char, 7, 1 ) = 1 then c_ratio else 1 end )
                           , utl_raw.copies( 'FF', case when substr( p_char, 8, 1 ) = 1 then c_ratio else 1 end )
                           , utl_raw.copies( '00', case when substr( p_char, 9, 1 ) = 1 then c_ratio else 1 end )
                           );
    end;
  begin
    t_fnt := cp1252_font;
--
    t_start_stop := char_bars( '010010100' );
    t_bars( 48 ) := char_bars( '000110100' );
    t_bars( 49 ) := char_bars( '100100001' );
    t_bars( 50 ) := char_bars( '001100001' );
    t_bars( 51 ) := char_bars( '101100000' );
    t_bars( 52 ) := char_bars( '000110001' );
    t_bars( 53 ) := char_bars( '100110000' );
    t_bars( 54 ) := char_bars( '001110000' );
    t_bars( 55 ) := char_bars( '000100101' );
    t_bars( 56 ) := char_bars( '100100100' );
    t_bars( 57 ) := char_bars( '001100100' );
--
    t_bars( 65 ) := char_bars( '100001001' );
    t_bars( 66 ) := char_bars( '001001001' );
    t_bars( 67 ) := char_bars( '101001000' );
    t_bars( 68 ) := char_bars( '000011001' );
    t_bars( 69 ) := char_bars( '100011000' );
    t_bars( 70 ) := char_bars( '001011000' );
    t_bars( 71 ) := char_bars( '000001101' );
    t_bars( 72 ) := char_bars( '100001100' );
    t_bars( 73 ) := char_bars( '001001100' );
    t_bars( 74 ) := char_bars( '000011100' );
    t_bars( 75 ) := char_bars( '100000011' );
    t_bars( 76 ) := char_bars( '001000011' );
    t_bars( 77 ) := char_bars( '101000010' );
    t_bars( 78 ) := char_bars( '000010011' );
    t_bars( 79 ) := char_bars( '100010010' );
    t_bars( 80 ) := char_bars( '001010010' );
    t_bars( 81 ) := char_bars( '000000111' );
    t_bars( 82 ) := char_bars( '100000110' );
    t_bars( 83 ) := char_bars( '001000110' );
    t_bars( 84 ) := char_bars( '000010110' );
    t_bars( 85 ) := char_bars( '110000001' );
    t_bars( 86 ) := char_bars( '011000001' );
    t_bars( 87 ) := char_bars( '111000000' );
    t_bars( 88 ) := char_bars( '010010001' );
    t_bars( 89 ) := char_bars( '110010000' );
    t_bars( 90 ) := char_bars( '011010000' );
--
    t_bars( 32 ) := char_bars( '011000100' );
    t_bars( 36 ) := char_bars( '010101000' );
    t_bars( 37 ) := char_bars( '000101010' );
    t_bars( 43 ) := char_bars( '010001010' );
    t_bars( 45 ) := char_bars( '010000101' );
    t_bars( 46 ) := char_bars( '110000100' );
    t_bars( 47 ) := char_bars( '010100010' );
--
    t_line := '00'; -- filter type None
    t_line := utl_raw.concat( t_line, t_start_stop );
    for c in 1 .. length( p_val )
    loop
      t_line := utl_raw.concat( t_line, 'FF', t_bars( ascii( substr( p_val, c ) ) ) );
    end loop;
    t_line := utl_raw.concat( t_line, 'FF', t_start_stop );
    t_width := utl_raw.length( t_line ) - 1;
--
    dbms_lob.createtemporary( t_dat, true, dbms_lob.call );
    for i in 1 .. t_height
    loop
      dbms_lob.writeappend( t_dat, t_width + 1, t_line );
    end loop;
--
    t_post := t_width - 8 * length( p_val ) - 8 - 4 - 4 - 8;
    t_pre :=  t_post / 2;
    t_post := t_post - t_pre;
    for j in 0 .. 13
    loop
      t_line := utl_raw.concat( '00' -- filter type None
                              , utl_raw.copies( 'FF', t_pre )
                              , utl_raw.substr( t_fnt, ( 42 * 14 + j ) * 8 + 1, 8 )
                              , utl_raw.copies( 'FF', 4 )
                              );
      for c in 1 .. length( p_val )
      loop
        t_line := utl_raw.concat( t_line
                                , utl_raw.substr( t_fnt, ( ascii( substr( p_val, c ) ) * 14 + j ) * 8 + 1, 8 )
                                );
      end loop;
      t_line := utl_raw.concat( t_line
                              , utl_raw.copies( 'FF', 4 )
                              , utl_raw.substr( t_fnt, ( 42 * 14 + j ) * 8 + 1, 8 )
                              , utl_raw.copies( 'FF', t_post )
                              );
      dbms_lob.writeappend( t_dat, t_width + 1, t_line );
    end loop;
--
    return generate_png( t_dat, t_width, t_height + 14 );
  end;
--
  function code128( p_val raw )
  return raw
  is
    c_factor constant pls_integer := 2;
    t_dat blob;
    t_line raw(32767);
    t_width pls_integer;
    t_height pls_integer := 45;
    t_start_stop raw(18);
    type tp_bars is table of raw(11) index by pls_integer;
    t_bars tp_bars;
    t_idx pls_integer;
    t_char pls_integer;
    t_check_digit number;
    t_pre pls_integer;
    t_post pls_integer;
    t_fnt raw(32767);
    t_bar_def varchar2(300) :=
      'H4sIAAAAAAAAC3VT0RLAIAji/3+a3ZoianvoKhQ05gAC9DWvZFzi+AUoGAsZiblt' ||
      'aopHLaXDmacHwKvBj9VzSd3yt6hHhHSh3tOsSq+TWuTPDlkQHtgx9DZCJ12QoaiH' ||
      'SgxKKieV4GWGFxgBEi2nNMnBxE/xlnjTlgtnpcyLM5B4pAYoLKgZS1Sw2BLiVi5O' ||
      'MVYBE+2kAL5Yd4Ci5YNiaqrf+j305gU56QYNScrA284qognSZHkPhnhoEPrnGkPQ' ||
      'QsPc/pFOlQegSmJnjgQAAA==';
    t_tmp raw(32767);
  begin
    t_fnt := cp1252_font;
    t_tmp := utl_compress.lz_uncompress( utl_encode.base64_decode( utl_raw.cast_to_raw( t_bar_def ) ) );
    for i in 0 .. 105
    loop
      t_bars( i ) := utl_raw.substr( t_tmp, i * 11 + 1, 11 );
    end loop;
--
    t_line := utl_raw.copies( 'FF', 10 ); -- quiet zone
    t_line := utl_raw.concat( t_line, t_bars( 104 ) ); -- Start Code B
    t_check_digit := 104;
    t_idx := 1;
    for c in 1 .. utl_raw.length( p_val )
    loop
      t_char := to_number( utl_raw.substr( p_val, c, 1 ), 'xx' );
      if t_char > 127
      then
        t_line := utl_raw.concat( t_line, t_bars( 100 ) ); -- FNC 4
        t_check_digit := t_check_digit + 100 * t_idx;
        t_char := t_char - 128;
        t_idx := t_idx + 1;
      end if;
      if t_char between 32 and 127
      then
        t_line := utl_raw.concat( t_line, t_bars( t_char - 32 ) );
        t_check_digit := t_check_digit + ( t_char - 32 ) * t_idx;
      elsif t_char < 32
      then
        t_line := utl_raw.concat( t_line, t_bars( 98 ) ); -- Shift A
        t_check_digit := t_check_digit + 98 * t_idx;
        t_idx := t_idx + 1;
        t_line := utl_raw.concat( t_line, t_bars( t_char ) );
        t_check_digit := t_check_digit + ( t_char ) * t_idx;
      end if;
      t_idx := t_idx + 1;
    end loop;
    t_line := utl_raw.concat( t_line, t_bars( mod( t_check_digit, 103 ) ) );
    t_line := utl_raw.concat( t_line, '0000FFFFFF000000FF00FF0000' ); -- Stop
    t_line := utl_raw.concat( t_line, utl_raw.copies( 'FF', 10 ) ); -- quiet zone
    if c_factor > 1
    then
      t_tmp := null;
      for i in 1 .. utl_raw.length( t_line )
      loop
        t_tmp := utl_raw.concat( t_tmp, utl_raw.copies( utl_raw.substr( t_line, i, 1 ), c_factor ) );
      end loop;
      t_line := t_tmp;
    end if;
    t_width := utl_raw.length( t_line );
    t_line := utl_raw.concat( '00', t_line ); -- filter type None
--
    dbms_lob.createtemporary( t_dat, true, dbms_lob.call );
    for i in 1 .. t_height
    loop
      dbms_lob.writeappend( t_dat, t_width + 1, t_line );
    end loop;
--
    t_post := t_width - 8 * utl_raw.length( p_val );
    t_pre :=  t_post / 2;
    t_post := t_post - t_pre;
    for i in 0 .. 13
    loop
      t_line := utl_raw.concat( '00' -- filter type None
                              , utl_raw.copies( 'FF', t_pre )
                              );
      for c in 1 .. utl_raw.length( p_val )
      loop
        t_line := utl_raw.concat( t_line
                                , utl_raw.substr( t_fnt, ( to_number( utl_raw.substr( p_val, c, 1 ), 'XX' ) * 14 + i ) * 8 + 1, 8 )
                                );
      end loop;
      t_line := utl_raw.concat( t_line, utl_raw.copies( 'FF', t_post ) );
      dbms_lob.writeappend( t_dat, t_width + 1, t_line );
    end loop;
--
    return generate_png( t_dat, t_width, t_height + 14 );
  end;
--
  function itf( p_val varchar2, p_parm varchar2 )
  return raw
  is
    c_quiet constant pls_integer := 10;
    c_ratio constant pls_integer := 3; -- 2 or 3
    t_val varchar2(2000);
    t_dat raw(32767);
    t_line raw(3999);
    t_height pls_integer := 40;
    t_fnt raw(1000);
    t_idx1 pls_integer;
    t_idx2 pls_integer;
    t_w pls_integer;
    t_ll pls_integer;
    t_pre pls_integer;
    t_post pls_integer;
    t_space pls_integer := 1;
    t_check pls_integer;
    t_bearer raw(100);
    t_def varchar2(50) := 'nnWWnWnnnWnWnnWWWnnnnnWnWWnWnnnWWnnnnnWWWnnWnnWnWn';
  begin
--
    t_fnt := number_font;
--
    t_val := trim( p_val );
    if instr( upper( p_parm ), 'C' ) > 0
    then
      if mod( length( t_val ), 2 ) = 0
      then
        t_val := '0' || t_val;
      end if;
      t_check := 0;
      for i in 1 .. ceil( length( t_val ) / 2 )
      loop
        t_check :=  t_check + substr( t_val, i * 2 - 1, 1 );
      end loop;
      t_check :=  t_check * 3;
      for i in 1 .. trunc( length( t_val ) / 2 )
      loop
        t_check :=  t_check + substr( t_val, i * 2, 1 );
      end loop;
      t_check := mod( t_check, 10 );
      if t_check > 0
      then
        t_check := 10 - t_check;
      end if;
      t_val := t_val || t_check;
    else
      if mod( length( t_val ), 2 ) = 1
      then
        t_val := '0' || t_val;
      end if;
    end if;
--
    t_ll := ( c_ratio * 2 + 3 ) * length( t_val ) + c_quiet * 2 + 6 + c_ratio;
    if instr( upper( p_parm ), 'B' ) > 0
    then
      t_ll := t_ll + c_ratio * 4;
      t_line := utl_raw.copies( '00', t_ll );
      t_line := utl_raw.concat( '00', t_line ); -- filter type None
      for i in 1 .. c_ratio * 2
      loop
        t_dat := utl_raw.concat( t_dat, t_line );
      end loop;
      if instr( p_parm, 'B' ) > 0
      then
        t_bearer := utl_raw.copies( '00', c_ratio * 2 );
      else
        t_bearer := utl_raw.copies( 'FF', c_ratio * 2 );
      end if;
    end if;
--
    t_line := utl_raw.concat( t_bearer
                            , utl_raw.copies( 'FF', c_quiet ) -- quiet zone
                            , '00FF00FF' -- Start Code
                            );
    for c in 1 .. length( t_val ) / 2
    loop
      t_idx1 := substr( t_val, c * 2 - 1, 1 );
      t_idx2 := substr( t_val, c * 2, 1 );
      for i in 1 .. 5
      loop
        t_w := case substr( t_def, t_idx1 * 5 + i, 1 )
                 when 'n' then 1 else c_ratio
               end;
        t_line := utl_raw.concat( t_line, utl_raw.copies( '00', t_w ) );
        t_w := case substr( t_def, t_idx2 * 5 + i, 1 )
                 when 'n' then 1 else c_ratio
               end;
        t_line := utl_raw.concat( t_line, utl_raw.copies( 'FF', t_w ) );
      end loop;
    end loop;
    t_line := utl_raw.concat( t_line
                            , utl_raw.copies( '00', c_ratio ), 'FF00' -- Stop Code
                            , utl_raw.copies( 'FF', c_quiet )         -- quiet zone
                            , t_bearer
                            );
    t_line := utl_raw.concat( '00', t_line ); -- filter type None
--
    for i in 1 .. t_height
    loop
      t_dat := utl_raw.concat( t_dat, t_line );
    end loop;
--
    if instr( upper( p_parm ), 'B' ) > 0
    then
      t_line := utl_raw.copies( '00', t_ll );
      t_line := utl_raw.concat( '00', t_line ); -- filter type None
      for i in 1 .. c_ratio * 2
      loop
        t_dat := utl_raw.concat( t_dat, t_line );
      end loop;
    end if;
--
    if instr( upper( p_parm ), 'H' ) > 0
    then
      for i in 1 .. t_space
      loop
        t_dat := utl_raw.concat( t_dat
                               , '00' -- filter type None
                               , utl_raw.copies( 'FF', t_ll )
                               );
      end loop;
--
      t_pre := t_ll - 7 * length( t_val );
      for j in 0 .. 8
      loop
        t_dat := utl_raw.concat( t_dat
                               , '00' -- filter type None
                               , utl_raw.copies( 'FF', trunc( t_pre / 2 ) )
                               );
        for c in 1 .. length( t_val )
        loop
          t_idx1 := substr( t_val, c, 1 );
          t_dat := utl_raw.concat( t_dat
                                 , utl_raw.substr( t_fnt, ( t_idx1 * 9 + j ) * 7 + 1, 7 )
                                 );
        end loop;
        t_dat := utl_raw.concat( t_dat
                               , utl_raw.copies( 'FF', t_pre - trunc( t_pre / 2 ) )
                               );
      end loop;
--
    end if;
--
    return generate_png( t_dat
                       , utl_raw.length( t_line ) - 1
                       , utl_raw.length( t_dat ) / utl_raw.length( t_line )
                       );
  end;
--
  function qrcode( p_val varchar2, p_eclevel varchar2 )
  return raw
  is
    t_version pls_integer;
    t_width pls_integer;
    t_line raw(200);
    type tp_matrix_row is table of pls_integer index by pls_integer;
    type tp_matrix is table of tp_matrix_row index by pls_integer;
    t_matrix tp_matrix;
    t_pat tp_matrix;
    t_tmp_row raw(32767);
    t_dat raw(32767);
    g_stream raw(32767);
    g_free pls_integer;
    t_tmp raw(8024);
    c_finder constant varchar2(49) :=
      '1111111100000110111011011101101110110000011111111';
    c_alignm constant varchar2(25) :=
      '1111110001101011000111111';
--
    procedure add_bits
      ( p_add raw
      , p_bits pls_integer
      )
    is
      t_tmp raw(4);
      t_len number;
      t_last raw(1);
    begin
      if g_stream is null or g_free is null
      then
        g_free := 8;
        t_len := 0;
      else
        t_len := utl_raw.length( g_stream );
      end if;
      t_tmp := utl_raw.substr( utl_raw.concat( '000000', p_add ), -4 );
      for i in reverse 1 .. p_bits
      loop
        if g_free = 8
        then
          g_stream := utl_raw.concat( g_stream, '00' );
          t_len := t_len + 1;
        end if;
        if dbms_utility.is_bit_set( t_tmp, i ) = 1
        then
          t_last := utl_raw.bit_or( utl_raw.substr( g_stream, -1 )
                                  , case g_free
                                      when 8 then '80'
                                      when 7 then '40'
                                      when 6 then '20'
                                      when 5 then '10'
                                      when 4 then '08'
                                      when 3 then '04'
                                      when 2 then '02'
                                      when 1 then '01'
                                    end
                                  );
          if t_len = 1
          then
            g_stream := t_last;
          else
            g_stream := utl_raw.concat( utl_raw.substr( g_stream, 1, t_len - 1 )
                                      , t_last
                                      );
          end if;
        end if;
        g_free := g_free - 1;
        if g_free = 0
        then
          g_free := 8;
        end if;
      end loop;
    end;
--
    function get_formatinfo( p_mask pls_integer )
    return pls_integer
    is
      t_p1 pls_integer;
      t_p2 pls_integer;
      t_tab varchar2(164) := ',77c4,72f3,7daa,789d,662f,6318,6c41,6976'
                          || ',5412,5125,5e7c,5b4b,45f9,40ce,4f97,4aa0'
                          || ',355f,3068,3f31,3a06,24b4,2183,2eda,2bed'
                          || ',1689,13be,1ce7,19d0,0762,0255,0d0c,083b,';
    begin
      t_p1 := instr( t_tab, ',', 1
                   , case nvl( upper( p_eclevel ), 'M' )
                       when 'L' then 0
                       when 'M' then 8
                       when 'Q' then 8 * 2
                       else 8 * 3
                     end
                   + p_mask + 1
                   ) + 1;
      t_p2 := instr( t_tab, ',', t_p1 );
      return to_number( substr( t_tab, t_p1, t_p2 - t_p1 ), 'xxxx' );
    end;
--
    function get_versioninfo( p_version pls_integer, p_var pls_integer )
    return pls_integer
    is
      t_p1 pls_integer;
      t_p2 pls_integer;
      t_rv varchar2(100);
      t_tab varchar2(800) := ',  26, 0,,  44,12,,  70,16,, 100,20,'
                          || ', 134,24,, 172,28,, 196,16,07c94'
                          || ', 242,18,085bc, 292,20,09a99, 346,22,0a4d3'
                          || ', 404,24,0bbf6, 466,26,0c762, 532,28,0d847'
                          || ', 581,20,0e60d, 655,22,0f928, 733,24,10b78'
                          || ', 815,24,1145d, 901,26,12a17, 991,28,13532'
                          || ',1085,28,149a6,1156,22,15683,1258,24,168c9'
                          || ',1364,24,177ec,1474,26,18ec4,1588,26,191e1'
                          || ',1706,28,1afab,1828,28,1b08e,1921,24,1cc1a'
                          || ',2051,24,1d33f,2185,26,1ed75,2323,26,1f250'
                          || ',2465,26,209d5,2611,28,216f0,2761,28,228ba'
                          || ',2876,24,2379f,3034,26,24b0b,3196,26,2542e'
                          || ',3362,26,26a64,3532,28,27541,3706,28,28c69,';
    begin
      t_p1 := instr( t_tab, ',', 1, ( p_version - 1 ) * 3 + p_var ) + 1;
      t_p2 := instr( t_tab, ',', t_p1 );
      t_rv := substr( t_tab, t_p1, t_p2 - t_p1 );
      return case when p_var = 3 then to_number( t_rv, 'xxxxx' ) else t_rv end;
    end;
--
    function get_config( p_version pls_integer, p_var pls_integer )
    return pls_integer
    is
      t_p1 pls_integer;
      t_p2 pls_integer;
/* Table 7  Number of symbol characters and input data capacity for QR Code 2005
Number of data codewords,
Data capacity Numeric,
Data capacity Alphanumeric,
Data capacity Byte
*/
      t_tab7 varchar2(32767) := ',  19,  41,  25,  17, 1, 0' -- 1
                             || ',  16,  34,  20,  14, 1, 0'
                             || ',  13,  27,  16,  11, 1, 0'
                             || ',   9,  17,  10,   7, 1, 0'
                             || ',  34,  77,  47,  32, 1, 0' -- 2
                             || ',  28,  63,  38,  26, 1, 0'
                             || ',  22,  48,  29,  20, 1, 0'
                             || ',  16,  34,  20,  14, 1, 0'
                             || ',  55, 127,  77,  53, 1, 0' -- 3
                             || ',  44, 101,  61,  42, 1, 0'
                             || ',  34,  77,  47,  32, 2, 0'
                             || ',  26,  58,  35,  24, 2, 0'
                             || ',  80, 187, 114,  78, 1, 0' -- 4
                             || ',  64, 149,  90,  62, 2, 0'
                             || ',  48, 111,  67,  46, 2, 0'
                             || ',  36,  82,  50,  34, 4, 0'
                             || ', 108, 255, 154, 106, 1, 0' -- 5
                             || ',  86, 202, 122,  84, 2, 0'
                             || ',  62, 144,  87,  60, 2, 2'
                             || ',  46, 106,  64,  44, 2, 2'
                             || ', 136, 322, 195, 134, 2, 0' -- 6
                             || ', 108, 255, 154, 106, 4, 0'
                             || ',  76, 178, 108,  74, 4, 0'
                             || ',  60, 139,  84,  58, 4, 0'
                             || ', 156, 370, 224, 154, 2, 0' -- 7
                             || ', 124, 293, 178, 122, 4, 0'
                             || ',  88, 207, 125,  86, 2, 4'
                             || ',  66, 154,  93,  64, 4, 1'
                             || ', 194, 461, 279, 192, 2, 0' -- 8
                             || ', 154, 365, 221, 152, 2, 2'
                             || ', 110, 259, 157, 108, 4, 2'
                             || ',  86, 202, 122,  84, 4, 2'
                             || ', 232, 552, 335, 230, 2, 0' -- 9
                             || ', 182, 432, 262, 180, 3, 2'
                             || ', 132, 312, 189, 130, 4, 4'
                             || ', 100, 235, 143,  98, 4, 4'
                             || ', 274, 652, 395, 271, 2, 2' -- 10
                             || ', 216, 513, 311, 213, 4, 1'
                             || ', 154, 364, 221, 151, 6, 2'
                             || ', 122, 288, 174, 119, 6, 2'
                             || ', 324, 772, 468, 321, 4, 0' -- 11
                             || ', 254, 604, 366, 251, 1, 4'
                             || ', 180, 427, 259, 177, 4, 4'
                             || ', 140, 331, 200, 137, 3, 8'
                             || ', 370, 883, 535, 367, 2, 2' -- 12
                             || ', 290, 691, 419, 287, 6, 2'
                             || ', 206, 489, 296, 203, 4, 6'
                             || ', 158, 374, 227, 155, 7, 4'
                             || ', 428,1022, 619, 425, 4, 0' -- 13
                             || ', 334, 796, 483, 331, 8, 1'
                             || ', 244, 580, 352, 241, 8, 4'
                             || ', 180, 427, 259, 177,12, 4'
                             || ', 461,1101, 667, 458, 3, 1' -- 14
                             || ', 365, 871, 528, 362, 4, 5'
                             || ', 261, 621, 376, 258,11, 5'
                             || ', 197, 468, 283, 194,11, 5'
                             || ', 523,1250, 758, 520, 5, 1' -- 15
                             || ', 415, 991, 600, 412, 5, 5'
                             || ', 295, 703, 426, 292, 5, 7'
                             || ', 223, 530, 321, 220,11, 7'
                             || ', 589,1408, 854, 586, 5, 1' -- 16
                             || ', 453,1082, 656, 450, 7, 3'
                             || ', 325, 775, 470, 322,15, 2'
                             || ', 253, 602, 365, 250, 3,13'
                             || ', 647,1548, 938, 644, 1, 5' -- 17
                             || ', 507,1212, 734, 504,10, 1'
                             || ', 367, 876, 531, 364, 1,15'
                             || ', 283, 674, 408, 280, 2,17'
                             || ', 721,1725,1046, 718, 5, 1' -- 18
                             || ', 563,1346, 816, 560, 9, 4'
                             || ', 397, 948, 574, 394,17, 1'
                             || ', 313, 746, 452, 310, 2,19'
                             || ', 795,1903,1153, 792, 3, 4' -- 19
                             || ', 627,1500, 909, 624, 3,11'
                             || ', 445,1063, 644, 442,17, 4'
                             || ', 341, 813, 493, 338, 9,16'
                             || ', 861,2061,1249, 858, 3, 5' -- 20
                             || ', 669,1600, 970, 666, 3,13'
                             || ', 485,1159, 702, 482,15, 5'
                             || ', 385, 919, 557, 382,15,10'
                             || ',';
    begin
      t_p1 := instr( t_tab7, ',', 1
                   , ( p_version - 1 ) * 4 * 6
                   + case nvl( upper( p_eclevel ), 'M' )
                       when 'L' then 0
                       when 'M' then 6
                       when 'Q' then 2 * 6
                       else 3 * 6
                     end
                   + p_var
                   ) + 1;
      t_p2 := instr( t_tab7, ',', t_p1 );
      return substr( t_tab7, t_p1, t_p2 - t_p1 );
    end;
--
    function add_error_correction( p_data raw )
    return raw
    is
      dataBlockSize pls_integer;
      eccBlockSize pls_integer;
      type tp_ecc is table of pls_integer index by pls_integer;
      gf tp_ecc; -- Galois Field Generation
      gfrev tp_ecc;
      j pls_integer;
      gp tp_ecc; -- Generator Polynomials
      gpi tp_ecc;
      t_ecc tp_ecc;
      t_first pls_integer;
      type tp_block is table of raw(80) index by pls_integer;
      t_blocks tp_block;
      t_ecc_blocks tp_block;
      t_cnt pls_integer;
      t_done pls_integer;
      t_rv raw(32767);
    begin
      dataBlockSize := trunc( get_config( t_version, 1 ) / ( get_config( t_version, 5 ) + get_config( t_version, 6 ) ) );
      eccBlockSize := trunc( ( get_versioninfo( t_version, 1 ) - get_config( t_version, 1 ) ) / ( get_config( t_version, 5 ) + get_config( t_version, 6 ) ) );
--
      for i in 0 .. get_config( t_version, 5 ) - 1
      loop
        t_blocks( i ) := utl_raw.substr( p_data, i * dataBlockSize + 1, dataBlockSize );
      end loop;
      t_cnt := t_blocks.count;
      t_done := t_cnt * dataBlockSize + 1;
      for i in 0 .. get_config( t_version, 6 ) - 1
      loop
        t_blocks( i + t_cnt ) := utl_raw.substr( p_data, i * ( dataBlockSize + 1 ) + t_done, dataBlockSize + 1 );
      end loop;
--
      j := 1;
      for i in 0 .. 254
      loop
        gf(i) := j;
        gfrev(j) := i;
        j := j * 2;
        if j > 255
        then
          j := bitxor( 285, j );
        end if;
      end loop;
--
      gp(0) := 1;
      for i in 0 .. eccBlockSize- 1
      loop
        gp(i+1) := 1;
        for j in reverse 1 .. i
        loop
          if gp(j) > 0
          then
            gp(j) := bitxor( gp(j-1), gf( mod( gfrev(gp(j)) + i, 255 ) ) ) ;
          else
            gp(j) := gp(j-1);
          end if;
        end loop;
        gp(0) := gf( mod( gfrev(gp(0)) + i, 255 ) );
      end loop;
      for i in gp.first .. gp.last
      loop
        gpi( gp.last - i ) := gp(i);
      end loop;
--
      for d in t_blocks.first .. t_blocks.last
      loop
        t_ecc.delete;
        for i in 1 .. utl_raw.length( t_blocks(d) )
        loop
          t_ecc( i ) := to_number( utl_raw.substr( t_blocks(d), i, 1 ), 'xx' );
        end loop;
        t_cnt := t_ecc.count;
        for i in 1 .. eccBlockSize
        loop
          t_ecc( t_cnt + i ) := 0;
        end loop;
/*
dbms_output.put('eccb ' || d || ' :' );
for i in t_ecc.first .. t_ecc.last
loop
dbms_output.put(t_ecc(i) || ',');
end loop;
dbms_output.put_line('');
*/
        while t_ecc.count >= gpi.count
        loop
          t_first := t_ecc( t_ecc.first );
--dbms_output.put_line( t_ecc.count || ' ' || t_ecc.first || ' ' || t_first );
          for i in 0 .. gpi.count - 1
          loop
--dbms_output.put_line(i || ' ' || gfrev( t_first ) );
            if t_first > 0
            then
              t_ecc( t_ecc.first + i ) := bitxor( t_ecc( t_ecc.first + i )
                                                , gf( mod( gfrev( gpi( i ) ) + gfrev( t_first ), 255 ) )
                                                );
            end if;
          end loop;
          t_ecc.delete( t_ecc.first );
        end loop;
--
        t_rv := null;
        for i in t_ecc.first .. t_ecc.last
        loop
          t_rv := utl_raw.concat( t_rv, to_char( t_ecc( i ), 'fm0X' ) );
        end loop;
        t_ecc_blocks( d ) := t_rv;
      end loop;
--
      t_rv := null;
      for i in 1 .. dataBlockSize
      loop
         for d in t_blocks.first .. t_blocks.last
         loop
           t_rv := utl_raw.concat( t_rv, utl_raw.substr( t_blocks(d), i, 1 ) );
        end loop;
      end loop;
      for d in get_config( t_version, 5 ) .. t_blocks.last
      loop
         t_rv := utl_raw.concat( t_rv, utl_raw.substr( t_blocks(d), -1 ) );
      end loop;
--
      for i in 1 .. eccBlockSize
      loop
         for d in t_blocks.first .. t_blocks.last
         loop
           t_rv := utl_raw.concat( t_rv, utl_raw.substr( t_ecc_blocks(d), i, 1 ) );
        end loop;
      end loop;
--
      return t_rv;
    end;
--
    procedure add_pat
      ( p_matrix in out tp_matrix
      , x pls_integer
      , y pls_integer
      , p_pat tp_matrix
      )
    is
    begin
      for r in p_pat.first .. p_pat.last
      loop
        for c in p_pat(p_pat.first).first .. p_pat(p_pat.first).last
        loop
          p_matrix( y + r - 1 )( x + c - 1 ) := p_pat( r )( c );
        end loop;
      end loop;
    end;
--
    procedure add_pat
      ( p_matrix in out tp_matrix
      , x pls_integer
      , y pls_integer
      , p_pat varchar2
      , rowlen pls_integer
      )
    is
      t_pat tp_matrix;
    begin
      for r in 1 .. length( p_pat ) / rowlen
      loop
        for c in 1 .. rowlen
        loop
          t_pat( r )( c ) := substr( p_pat, ( r - 1 ) * rowlen + c, 1 );
        end loop;
      end loop;
      add_pat( p_matrix, x, y, t_pat );
    end;
--
    function mask_function
      ( f pls_integer
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
      , p_masked in out tp_matrix
      , p_mask pls_integer
      )
    is
      t_info pls_integer;
    begin
      for y in 0 .. t_width - 1
      loop
        for x in 0 .. t_width - 1
        loop
          if p_mat( y )( x ) > 127
          then
            p_masked( y )( x ) := bitxor( p_mat( y )( x ) - 128, mask_function( p_mask, y, x ) );
          else
            p_masked( y )( x ) := p_mat( y )( x );
          end if;
        end loop;
      end loop;
      t_info := get_formatinfo( p_mask );
      for i in 0 .. 5
      loop
        p_masked(i)(8) := case when bitand( t_info, power( 2, i ) ) > 0 then 1 else 0 end;
        p_masked(8)(i) := case when bitand( t_info, power( 2, 14 - i ) ) > 0 then 1 else 0 end;
      end loop;
      p_masked(7)(8) := case when bitand( t_info, power( 2, 6 ) ) > 0 then 1 else 0 end;
      p_masked(8)(8) := case when bitand( t_info, power( 2, 7 ) ) > 0 then 1 else 0 end;
      p_masked(8)(7) := case when bitand( t_info, power( 2, 8 ) ) > 0 then 1 else 0 end;
      for i in 0 .. 6
      loop
        p_masked(8)(t_width-1-i) := case when bitand( t_info, power( 2, i ) ) > 0 then 1 else 0 end;
        p_masked(t_width-1-i)(8) := case when bitand( t_info, power( 2, 14 - i ) ) > 0 then 1 else 0 end;
      end loop;
      p_masked(8)(t_width-8) := case when bitand( t_info, power( 2, 7 ) ) > 0 then 1 else 0 end;
    end;
--
  begin
    t_version := 1;
    if translate( p_val, '#0123456789', '#' ) is null
    then  -- numeric mode
      t_version := 1;
      while length( p_val ) > get_config( t_version, 2 )
      loop
        t_version := t_version + 1;
      end loop;
dbms_output.put_line( 'numeric: ' || t_version );
      add_bits( '01', 4 ); -- mode
      add_bits( to_char( length( p_val ), 'fm0XXX' )
              , case
                  when t_version <= 9 then 10
                  when t_version <= 26 then 12
                  else 14
                end
              );
      for i in 1 .. trunc( length( p_val ) / 3 )
      loop
        add_bits( to_char( substr( p_val, i * 3 - 2, 3 ), 'fm0XX' ), 10 );
      end loop;
      case mod( length( p_val ), 3 )
        when 1 then add_bits( to_char( substr( p_val, -1 ), 'fm0X' ), 4 );
        when 2 then add_bits( to_char( substr( p_val, -2 ), 'fm0X' ), 7 );
        else null;
      end case;
    elsif translate( p_val, '#0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:', '#' ) is null
    then -- alphanumeric mode
      t_version := 1;
      while length( p_val ) > get_config( t_version, 3 )
      loop
        t_version := t_version + 1;
      end loop;
dbms_output.put_line( 'alphanumeric: ' || t_version );
      add_bits( '02', 4 ); -- mode
      add_bits( to_char( length( p_val ), 'fm0XXX' )
              , case
                  when t_version <= 9 then 9
                  when t_version <= 26 then 11
                  else 13
                end
              );
      t_tmp := utl_raw.translate( utl_raw.cast_to_raw( p_val )
                                , utl_raw.concat( utl_raw.xrange( '30', '39' )
                                                , utl_raw.xrange( '41', '5A' )
                                                , '2024252A2B2D2E2F3A'
                                                )
                                , utl_raw.xrange( '00', '2C' )
                                );
      for i in 1 .. trunc( length( p_val ) / 2 )
      loop
        add_bits( to_char( to_number( utl_raw.substr( t_tmp, i * 2 - 1, 1 ), 'xx' ) * 45
                         + to_number( utl_raw.substr( t_tmp, i * 2, 1 ), 'xx' )
                         , 'fm00XX'
                         )
                , 11
                );
      end loop;
      case mod( length( p_val ), 2 )
        when 1 then add_bits( utl_raw.substr( t_tmp, -1 ), 6 );
        else null;
      end case;
    elsif utl_raw.compare( utl_i18n.string_to_raw( p_val, 'WE8ISO8859P1' )
                         , utl_i18n.string_to_raw( p_val, 'AL32UTF8' )
                         ) = 0
    then -- byte mode
      t_version := 1;
      while length( p_val ) > get_config( t_version, 4 )
      loop
        t_version := t_version + 1;
      end loop;
dbms_output.put_line( 'byte: ' || t_version );
      add_bits( '04', 4 ); -- mode
      case
        when t_version <= 9
          then add_bits( to_char( length( p_val ), 'fm0X' ), 8 );
          else add_bits( to_char( length( p_val ), 'fm0XXX' ), 16 );
      end case;
      t_tmp := utl_i18n.string_to_raw( p_val, 'WE8ISO8859P1' );
      for i in 1 .. utl_raw.length( t_tmp )
      loop
        add_bits( utl_raw.substr( t_tmp, i, 1 )
                , 8
                );
      end loop;
    else -- ECI mode
      add_bits( '07', 4 ); -- mode
      add_bits( '1A', 6 ); -- ECI Assignment number 26 = UTF8
      add_bits( '04', 4 ); -- byte mode
      case
        when t_version <= 9
          then add_bits( to_char( utl_raw.length( utl_i18n.string_to_raw( p_val, 'AL32UTF8' ) ), 'fm0X' ), 8 );
          else add_bits( to_char( utl_raw.length( utl_i18n.string_to_raw( p_val, 'AL32UTF8' ) ), 'fm0XXX' ), 16 );
      end case;
      t_tmp := utl_i18n.string_to_raw( p_val, 'AL32UTF8' ); -- TODO: escape 5C = \ ********************
      for i in 1 .. utl_raw.length( t_tmp )
      loop
        add_bits( utl_raw.substr( t_tmp, i, 1 )
                , 8
                );
      end loop;
    end if;
--
    dbms_output.put_line( g_free || ' ' || g_stream );
    add_bits( '00', 4 ); -- terminator
    g_stream := utl_raw.substr( utl_raw.concat( g_stream
                                              , utl_raw.copies( 'EC11', 1500 )
                                              )
                              , 1
                              , get_config( t_version, 1 )
                              ); -- padding codewords
    dbms_output.put_line( g_stream );
--
    g_stream := add_error_correction( g_stream );
    dbms_output.put_line( g_stream );
--
    t_width := t_version * 4 + 17;
    for r in 0 .. t_width - 1
    loop
      for c in 0 .. t_width - 1
      loop
        t_matrix( r )( c ) := 3;
      end loop;
    end loop;
    add_pat( t_matrix, 0, 0, c_finder, 7 );
    add_pat( t_matrix, 0, t_width - 7, c_finder, 7 );
    add_pat( t_matrix, t_width - 7, 0, c_finder, 7 );
    for i in 0 .. 7  -- separators
    loop
      t_matrix( 7 )( i ) := 0;
      t_matrix( i )( 7 ) := 0;
      t_matrix( t_width - 8 )( i ) := 0;
      t_matrix( t_width - 8 + i )( 7 ) := 0;
      t_matrix( 7 )( t_width - 8 + i ) := 0;
      t_matrix( i )( t_width - 8 ) := 0;
    end loop;
-- aligment
    declare
      t_offset pls_integer;
      t_appmax pls_integer;
      x pls_integer;
      y pls_integer;
    begin
      t_offset := get_versioninfo( t_version, 2 );
      if t_offset > 0
      then
        t_appmax := t_version * 4 + 10;
        y := t_appmax;
        loop
          x := t_appmax;
          loop
            if not (  ( x = 6 and y = 6 )
                   or ( x = 6 and y = t_width - 7 )
                   or ( x = t_width - 7 and y = 6 )
                   )
            then
              add_pat( t_matrix, x - 2, y - 2, c_alignm, 5 );
            end if;
            exit when x = 6;
            x := x - t_offset;
            if x < 18
            then
              x := 6;
            end if;
          end loop;
          exit when y = 6;
          y := y - t_offset;
          if y < 18
          then
            y := 6;
          end if;
        end loop;
      end if;
    end;
-- format information
    for i in 0 .. 7
    loop
      t_matrix( 8 )( i ) := 0;
      t_matrix( i )( 8 ) := 0;
      t_matrix( t_width - 8 + i )( 8 ) := 0;
      t_matrix( 8 )( t_width - 8 + i ) := 0;
    end loop;
    t_matrix( 8 )( 8 ) := 0;
    t_matrix( t_width - 8 )( 8 ) := 1;
-- version information
    declare
      t_tmp pls_integer;
      t_info pls_integer;
    begin
      if t_version >= 7
      then
        t_info := get_versioninfo( t_version, 3 );
        for i in 0 .. 5
        loop
          for j in 0 .. 2
          loop
            t_tmp := bitand( t_info, power( 2, i * 3 + j ) );
            t_tmp := case when t_tmp > 0 then 1 else 0 end;
            t_matrix( t_width - 11 + j )( i ) := t_tmp; -- lower left
            t_matrix( i )( t_width - 11 + j ) := t_tmp; -- upper right
          end loop;
        end loop;
      end if;
   end;
--
    for i in 8 .. t_width - 9  -- timing paterns
    loop
      t_matrix( 6 )( i ) := mod( i + 1, 2 );
      t_matrix( i )( 6 ) := mod( i + 1, 2 );
    end loop;
--
    declare
      x pls_integer;
      y pls_integer;
      dir pls_integer;
      function get_bit( p_val raw, p_bit pls_integer )
      return pls_integer
      is
      begin
        return case when bitand( to_number( p_val, 'XX' )
                               , power( 2, p_bit )
                               ) > 0
                 then 1
                 else 0
               end;
      end;
    begin
      dir := -1;
      x := t_width - 1;
      y := x;
      for i in 1 .. utl_raw.length( g_stream )
      loop
        for b in reverse 0 .. 7
        loop
--dbms_output.put_line( (( i - 1 ) * 8 + 7 - b )|| '  ' || x || ' ' || y || '  ' || get_bit( utl_raw.substr( g_stream, i, 1 ), b ) );
          t_matrix( y )( x ) := 128 + get_bit( utl_raw.substr( g_stream, i, 1 ), b );
          loop
            if (  ( x > 6 and mod( x, 2 ) = 0 )
               or ( x < 6 and mod( x, 2 ) = 1 )
               )
            then
              x := x - 1;
            else
              if (  ( dir = - 1 and y = 0 )
                 or ( dir = 1 and y = t_width - 1 )
                 )
              then
                exit when x = 0;
                dir := - dir;
                x := x - 1;
                if x = 6
                then
                  x := 5;
                end if;
              else
                y := y + dir;
                x := x + 1;
              end if;
            end if;
            exit when t_matrix( y )( x ) = 3;
          end loop;
        end loop;
      end loop;
    end;
--
    declare
      t_mask pls_integer;
      masked tp_matrix;
      n1 pls_integer;
      n2 pls_integer;
      n3 pls_integer;
      n4 pls_integer;
      best number;
      score number;
    begin
      best := 99999999;
      for m in 0 .. 7
      loop
        mask_matrix( t_matrix, masked, m );
        n1 := 0;
        n2 := 0;
        n3 := 0;
        n4 := 0;
        for y in 0 .. t_width - 1
        loop
          for x in 0 .. t_width - 1
          loop
            if ( x >= 6
               and ( masked(y)(x-6) + masked(y)(x-5) + masked(y)(x-4)
                   + masked(y)(x-3) + masked(y)(x-2) + masked(y)(x-1)
                   + masked(y)(x)
                   ) in ( 0, 7 )
               )
            then
              n1 := n1 + 1;
            end if;
            if ( y >= 6
               and ( masked(y-6)(x) + masked(y-5)(x) + masked(y-4)(x)
                   + masked(y-3)(x) + masked(y-2)(x) + masked(y-1)(x)
                   + masked(y)(x)
                   ) in ( 0, 7 )
               )
            then
              n1 := n1 + 1;
            end if;
--
            if ( x > 0 and y > 0
               and ( masked(y)(x) + masked(y)(x-1)
                   + masked(y-1)(x) + masked(y-1)(x-1)
                   ) in ( 0, 4 )
               )
            then
              n2 := n2 + 1;
            end if;
--
            if (   x >= 6
               and masked(y)(x-6) = 1
               and masked(y)(x-5) = 0
               and masked(y)(x-4) = 1
               and masked(y)(x-3) = 1
               and masked(y)(x-2) = 1
               and masked(y)(x-1) = 0
               and masked(y)(x-0) = 1
               )
            then
              n3 := n3 + 1;
            end if;
            if (   y >= 6
               and masked(y-6)(x) = 1
               and masked(y-5)(x) = 0
               and masked(y-4)(x) = 1
               and masked(y-3)(x) = 1
               and masked(y-2)(x) = 1
               and masked(y-1)(x) = 0
               and masked(y-0)(x) = 1
               )
            then
              n3 := n3 + 1;
            end if;
--
            n4 := n4 + masked(y)(x);
          end loop;
        end loop;
        score := n1 * 3 + n2 * 3 + n3 * 40;
        score := score + 2 * abs( ( 100 * n4 ) / ( t_width * t_width ) - 50 );
--dbms_output.put_line( n1 || ' ' || n2 || ' ' || n3 || ' ' || n4 || ' ' || score );
        if score < best
        then
          t_mask := m;
          best := score;
        end if;
      end loop;
      mask_matrix( t_matrix, t_matrix, t_mask );
--dbms_output.put_line( 'mask: ' || t_mask );
    end;
--
    for i in 1 .. 4  -- add quiet zone
    loop
      t_dat := utl_raw.concat( t_dat
                             , '00'-- filter type None
                             , utl_raw.copies( 'FF', t_width + 8 )
                             );
    end loop;
    for r in 0 .. t_width - 1
    loop
      t_tmp_row := null;
      for c in 0 .. t_width - 1
      loop
        t_tmp_row := utl_raw.concat( t_tmp_row
                                   , case t_matrix( r )( c )
                                       when 0   then 'FF'
                                       when 1   then '00'
                                       when 128 then 'FF'
                                       when 129 then '00'
                                       else 'FF' --'AA'
                                     end
                                   );
      end loop;
      t_dat := utl_raw.concat( t_dat
                             , '00'-- filter type None
                             , utl_raw.copies( 'FF', 4 )
                             , t_tmp_row
                             , utl_raw.copies( 'FF', 4 )
                             );
    end loop;
    for i in 1 .. 4  -- add quiet zone
    loop
      t_dat := utl_raw.concat( t_dat
                             , '00'-- filter type None
                             , utl_raw.copies( 'FF', t_width + 8 )
                             );
    end loop;
    declare
      t_tmp blob;
      t_line raw(1000);
      c_factor constant pls_integer := 2;
    begin
      dbms_lob.createtemporary( t_tmp, true, dbms_lob.call );
      t_width := t_width + 8;
      for i in 0 .. t_width - 1
      loop
        t_line := utl_raw.substr( t_dat, ( t_width + 1 ) * i + 1, 1 );
        for j in 2 .. t_width + 1
        loop
          t_line := utl_raw.concat( t_line
                                  , utl_raw.copies( utl_raw.substr( t_dat, ( t_width + 1 ) * i + j, 1 )
                                                  , c_factor
                                                  )
                                  );
        end loop;
        for j in 1 .. c_factor
        loop
          dbms_lob.writeappend( t_tmp, t_width * c_factor + 1, t_line );
        end loop;
      end loop;
      return generate_png( t_tmp, t_width * c_factor, t_width * c_factor );
    end;
    return generate_png( t_dat, t_width + 8, t_width + 8 );
  end;
--
  function aztec( p_val varchar2, p_param varchar2 := '' )
  return raw
  is
    t_dat raw(32767);
    t_width pls_integer;
    t_bits tp_bits;
    t_sbits tp_bits;
    t_modemsg tp_bits;
    t_ecc_size pls_integer;
    t_total_size pls_integer;
    t_layers pls_integer;
    t_bil pls_integer;
    t_wordsize pls_integer;
    t_compact boolean;
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
    procedure get_bits( p_val raw, p_bits in out tp_bits )
    is
      t_cur_mode pls_integer := 1;
      t_nxt_mode pls_integer;
      t_char pls_integer;
      t_nxt_char pls_integer;
      t_idx pls_integer;
      t_len pls_integer;
      t_codes tp_bits;
      t_modes tp_bits;
      t_x raw(128);
      t_y pls_integer;
      t_geg varchar2(250) := 
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
    begin
      t_x := utl_encode.base64_decode( utl_raw.cast_to_raw( t_geg ) );
      for i in 0 .. 127
      loop
        t_codes( i ) := 0;
        t_modes( i ) := 0;
        t_y := to_number( utl_raw.substr( t_x, i + 1, 1 ), 'xx' ); 
        t_codes( i ) := mod( t_y, 32 );
        t_modes( i ) := trunc( t_y / 32 );
      end loop;
--
      t_idx := 1;
      t_len := utl_raw.length( p_val );
      t_nxt_char := to_number( utl_raw.substr( p_val, 1, 1 ), 'XX' );
      loop
        exit when t_idx > t_len; 
        t_char := t_nxt_char;
        begin
          t_nxt_char := to_number( utl_raw.substr( p_val, t_idx + 1, 1 ), 'XX' );
        exception
          when others then t_nxt_char := 0;
        end;
        t_nxt_mode := t_modes( t_char );
        if t_nxt_mode = t_cur_mode
        then
          add_code( t_codes( t_char ), t_cur_mode );
        else
          if t_nxt_mode = 5
          then
            if t_char = 32
            then
              if    ( t_cur_mode = 2 and t_modes( t_nxt_char ) in ( 1, 5 ) )
                 or ( t_cur_mode = 1 and t_modes( t_nxt_char ) = 5 ) 
              then
                add_code( 30, 2 );
                t_cur_mode := 5;
              end if;
              add_code( t_codes( t_char ), t_cur_mode );
            elsif t_char in ( 44, 46 ) and t_nxt_char = 32
            then
              add_code( 0, 1 );
              if t_char = 44
              then
                add_code( 4, 1 );
              else
                add_code( 3, 1 );
              end if;
              t_idx := t_idx + 1;  
              begin
                t_nxt_char := to_number( utl_raw.substr( p_val, t_idx + 1, 1 ), 'XX' );
              exception
                when others then t_nxt_char := 0;
              end;
            else
              if t_cur_mode = 3
              then
                add_code( 29, 3 );
              elsif t_cur_mode = 4
              then
                add_code( 31, 4 );
              end if;
              add_code( 30, 1 );
              t_cur_mode := 5;
              add_code( t_codes( t_char ), t_cur_mode );
            end if;
          elsif t_nxt_mode = 4
          then
            add_code( 0, t_cur_mode );
            if t_char = 13 and t_nxt_char = 10
            then 
              add_code( 2, 4 );
              t_idx := t_idx + 1;  
              begin
                t_nxt_char := to_number( utl_raw.substr( p_val, t_idx + 1, 1 ), 'XX' );
              exception
                when others then t_nxt_char := 0;
              end;
            elsif t_char = 58 and t_nxt_char = 32
            then 
              add_code( 5, 4 );
              t_idx := t_idx + 1;  
              begin
                t_nxt_char := to_number( utl_raw.substr( p_val, t_idx + 1, 1 ), 'XX' );
              exception
                when others then t_nxt_char := 0;
              end;
            else
              add_code( t_codes( t_char ), 4 );
            end if;
          elsif t_nxt_mode = 3
          then
            if t_cur_mode = 5
            then
              add_code( 14, 5 );
            end if;
            add_code( 29, 1 );
            t_cur_mode := 3;
            add_code( t_codes( t_char ), t_cur_mode );
          elsif t_nxt_mode = 2
          then
            if t_cur_mode = 5
            then
              add_code( 14, 5 );
            end if;
            add_code( 28, 1 );
            t_cur_mode := 2;
            add_code( t_codes( t_char ), t_cur_mode );
          elsif t_nxt_mode = 1
          then
            if t_cur_mode = 3
            then
              add_code( 29, 3 );
              t_cur_mode := 1;
            elsif t_cur_mode = 2
            then
              case t_modes( t_nxt_char )
                when 2
                then
                  add_code( 28, 2 );
                when 5       
                then
                  add_code( 30, 2 );
                  t_cur_mode := 5;
                  add_code( 15, 5 );
                else
                  add_code( 30, 2 );
                  add_code( 14, 5 );
                  t_cur_mode := 1;
                end case;
            else -- t_cur_mode = 5
              if t_modes( t_nxt_char ) = 1
              then
                add_code( 14, 5 );
                t_cur_mode := 1;
              else 
                add_code( 15, 5 );
              end if;
            end if;
            add_code( t_codes( t_char ), 1 );
          else
            if t_cur_mode = 5
            then
              add_code( 14, 5 );
              t_cur_mode := 1;
            end if;
            add_code( 31, 1 );
            add_code( 1, 1 ); -- length 1 => 1 8 bit binary follows
            p_bits( p_bits.count ) := sign( bitand( t_char, 128 ) );
            p_bits( p_bits.count ) := sign( bitand( t_char, 64 ) );
            p_bits( p_bits.count ) := sign( bitand( t_char, 32 ) );
            p_bits( p_bits.count ) := sign( bitand( t_char, 16 ) );
            p_bits( p_bits.count ) := sign( bitand( t_char, 8 ) );
            p_bits( p_bits.count ) := sign( bitand( t_char, 4 ) );
            p_bits( p_bits.count ) := sign( bitand( t_char, 2 ) );
            p_bits( p_bits.count ) := sign( bitand( t_char, 1 ) );
          end if;
        end if;
        t_idx := t_idx + 1;  
      end loop; 
    end;
--
  function bitstoword( p_bits tp_bits, p_sz pls_integer )
  return tp_bits
  is
    t_val pls_integer;
    t_rv tp_bits;
    t_first pls_integer := p_bits.first; 
  begin
    for i in t_first .. p_bits.count / p_sz - t_first - 1
    loop
      t_val := 0;
      for j in 0 .. p_sz - 1
      loop
        t_val := t_val * 2 + p_bits( t_first + ( i - t_first ) * p_sz + j );
      end loop;
      t_rv( i - t_first ) := t_val;
    end loop;
    return t_rv; 
  end; 
--
  procedure show_bits( p_bits tp_bits, p_txt varchar2 )
  is
  begin
    dbms_output.put_line( p_txt || ' ' || p_bits.count );
    for i in 0 .. trunc( p_bits.count / 8 )
    loop
      for j in 0 .. 7
      loop
        dbms_output.put( case p_bits( i * 8 + j ) when 1 then 'X' else '.' end );
      end loop;
      dbms_output.put( ' ' );
    end loop;
    dbms_output.put_line( '' );
    exception when no_data_found then dbms_output.put_line( '' );
    end;
  begin
    get_bits( utl_i18n.string_to_raw( p_val, 'WE8ISO8859P1' ), t_bits );
--
    t_total_size := t_bits.count;
    t_ecc_size := trunc( t_total_size * nvl( p_param, '.33' ) ) + 11;
    t_total_size := t_total_size + t_ecc_size;
    dbms_output.put_line( 'ecc: ' || t_ecc_size || '  total: ' || t_total_size );
    show_bits( t_bits, 'bits:' );
    t_wordsize := 0;
    for i in 0 .. 33
    loop
      if i > 32
      then
        raise_application_error( -20000, 'Data too large for an Aztec code' ); 
      end if;
      t_compact := i <= 3; 
      t_layers := case when t_compact then i + 1 else i end;
      t_bil := case when t_compact then 88 else 112 end; 
      t_bil := ( t_bil + 16 * t_layers ) * t_layers; 
      if t_total_size <= t_bil
      then
        if t_wordsize != case when t_layers > 22 then 12
                              when t_layers > 8 then 10
                              when t_layers > 2 then 8
                              else 6
                         end
        then
          t_wordsize := case when t_layers > 22 then 12
                             when t_layers > 8 then 10
                             when t_layers > 2 then 8
                             else 6
                        end;
          declare
            i pls_integer := t_bits.first;
            t_last pls_integer := t_bits.last;
            t_word number;
            t_mask number := power( 2, t_wordsize ) - 2;
          begin
            t_sbits.delete;
            loop
              exit when i > t_last;
              t_word := 0;
              for j in 0 .. t_wordsize - 1
              loop
                t_word := t_word * 2;
                if i + j > t_last
                then
                  t_word := t_word + 1;
                else
                  t_word := t_word + t_bits( i + j );
                end if;
              end loop;
              case bitand( t_word, t_mask )
                when t_mask
                then
                  t_word := t_mask;
                  i := i - 1;
dbms_output.put_line( 'stuf1 ' || i ); 
                when 0
                then
                  t_word := t_word + 1 - bitand( t_word, 1 );
                  i := i - 1;
dbms_output.put_line( 'stuf2 ' || i ); 
                else
                  null;
              end case;
              for j in reverse 0 .. t_wordsize - 1
              loop
                t_sbits( t_sbits.count ) := sign( bitand( t_word, power( 2, j ) ) );
              end loop; 
              i := i + t_wordsize;
            end loop;
          end;
        end if;
        exit when (  ( not t_compact or t_sbits.count <= t_wordsize * 64 )
                  and t_sbits.count + t_ecc_size <= t_bil - mod( t_bil, t_wordsize )
                  );
      end if;       
    end loop;
    show_bits( t_sbits, 'sbits:' );
    dbms_output.put_line( t_layers || ' ' || t_wordsize || case when t_compact then ' compact' else ' normal' end );
--
    declare
      t_sz pls_integer;
      t_tmp tp_bits;
    begin
      t_sz := t_sbits.count / t_wordsize; 
      if t_compact
      then
        append_bits( t_modemsg, t_layers - 1, 2 );
        append_bits( t_modemsg, t_sz - 1, 6 );
        t_tmp := reed_solomon( bitstoword( t_modemsg, 4 ), 19, 16, 5 ); 
      else
        append_bits( t_modemsg, t_layers - 1, 5 );
        append_bits( t_modemsg, t_sz - 1, 11 );
        t_tmp := reed_solomon( bitstoword( t_modemsg, 4 ), 19, 16, 6 ); 
      end if;  
      show_bits( t_modemsg, 'mode:' );
      for i in t_tmp.first .. t_tmp.last
      loop
        append_bits( t_modemsg, t_tmp( i ), 4 );
      end loop;  
      show_bits( t_modemsg, 'mode:' );
--
      t_sz := ( t_bil - t_sbits.count ) / t_wordsize;
dbms_output.put_line( 'degree:' || t_sz ); 
      case t_wordsize
        when 6 then
          t_tmp := reed_solomon( bitstoword( t_sbits, t_wordsize ), 67, 64, t_sz ); 
        when 8 then
          t_tmp := reed_solomon( bitstoword( t_sbits, t_wordsize ), 301, 256, t_sz ); 
        when 10 then
          t_tmp := reed_solomon( bitstoword( t_sbits, t_wordsize ), 1033, 1024, t_sz ); 
        when 12 then
          t_tmp := reed_solomon( bitstoword( t_sbits, t_wordsize ), 4201, 4096, t_sz ); 
      end case;
dbms_output.put_line( 'padding:' || mod( t_bil, t_wordsize ) );
      if mod( t_bil, t_wordsize ) > 0
      then
        declare
          t_pad pls_integer := mod( t_bil, t_wordsize );
        begin 
          for i in reverse t_sbits.first .. t_sbits.last
          loop
            t_sbits( i + t_pad ) := t_sbits( i );
          end loop;
          for i in t_sbits.first .. t_sbits.first + t_pad - 1
          loop
            t_sbits( i ) := 0;
          end loop;
        end;  
      end if;  
      for i in t_tmp.first .. t_tmp.last
      loop
        append_bits( t_sbits, t_tmp( i ), t_wordsize );
      end loop;  
      show_bits( t_sbits, 'msg:' );
    end;
--
    t_width := 4 * t_layers + case when t_compact then 11 else 15 end;
    for i in 1 .. t_width
    loop
      t_dat := utl_raw.concat( t_dat, '00', utl_raw.copies( 'FF', t_width ) );
    end loop;
    declare
      t_cent number := ( t_width+1 ) * trunc( t_width/2 ) + 1 + ceil( t_width/2 );
      t_idx pls_integer;
      t_w pls_integer;
      t_st pls_integer;
      procedure draw( x pls_integer, y pls_integer, z pls_integer := 1 )
      is
      begin
        t_dat := utl_raw.overlay( case when z = 0 then 'FF' else '00' end
                                , t_dat, t_cent + x + y * ( t_width+1 )
                                ); 
      end;
      procedure dline( x pls_integer )
      is
      begin
        for i in -x .. x
        loop
          draw( i, -x );
          draw( i, x );
          draw( -x, i );
          draw( x, i );
        end loop;
      end;
      procedure dorient( x pls_integer )
      is
      begin
        draw( -x, -x );
        draw( -x + 1, -x );
        draw( -x, -x + 1 );
        draw( x, -x );
        draw( x, -x + 1 );
        draw( x, x - 1);
      end;
      procedure dmode( x pls_integer, y pls_integer, p_mode tp_bits )
      is
      begin
        for i in 1 .. y
        loop
          draw( -x + 1 + i, -x, p_mode( i - 1 ) );
          draw( x, -x + 1 + i, p_mode( i - 1 + y ) );
          draw( x - 1 - i, x, p_mode( i - 1 + 2 * y ) );
          draw( -x, x - 1 - i, p_mode( i - 1 + 3 * y ) );
        end loop;
      end;
    begin
      draw( 0, 0 );
      dline( 2 );
      dline( 4 );
      if t_compact
      then
        dorient( 5 ); 
        dmode( 5, 7, t_modemsg );
        t_idx := 0;
        for j in reverse 1 .. t_layers
        loop
          t_st := 2 * j + 5;
          t_w := t_st * 4 - 2;
          for i in 0 .. t_w - 1
          loop
            draw( -t_st + mod( i, 2 ), -t_st + trunc( i / 2 ), t_sbits( t_idx + i ) );          -- links
            draw( -t_st + trunc( i / 2 ), t_st - mod( i, 2 ), t_sbits( t_idx + t_w + i ) );     -- onder
            draw( t_st - mod( i, 2 ), t_st - trunc( i / 2 ), t_sbits( t_idx + t_w * 2 + i ) );  -- rechts
            draw( t_st - trunc( i / 2 ), -t_st + mod( i, 2 ), t_sbits( t_idx + t_w * 3 + i ) ); -- boven
          end loop;
          t_idx := t_idx + t_w * 4;
        end loop; 
      else
        dline( 6 );
        dorient( 7 ); 
      end if;
    end;
    return generate_png( t_dat
                       , t_width
                       , t_width
                       );
  end;
--
  function barcode( p_val varchar2, p_type varchar2, p_parm varchar2 := null )
  return raw
  is
    t_val varchar2(32767);
    t_tmp pls_integer;
  begin
    begin
      if p_val is not null
      then
        if upper( p_type ) in ( 'EAN8', 'EAN-8' )
        then
          if length( trim( p_val ) ) = 8
          then -- check digit included
            t_val := to_char( to_number( p_val ), 'fm09999999' );
            return ean8( t_val );
          elsif length( trim( p_val ) ) < 8
          then -- check digit not included
            t_val := to_char( to_number( p_val ), 'fm0999999' );
            t_tmp := 0;
            for i in 1 .. 7
            loop
              t_tmp := t_tmp + ( ascii( substr( t_val, i ) ) - 48 ) * case when mod( i, 2 ) = 0 then 1 else 3 end;
            end loop;
            t_val := t_val || to_char( mod( 10 - mod( t_tmp, 10 ), 10 ), 'fm0' );
            return ean8( t_val );
          end if;
        elsif upper( p_type ) in ( 'EAN13', 'EAN-13' )
        then
          if length( trim( p_val ) ) = 13
          then -- check digit included
            t_val := to_char( to_number( p_val ), 'fm0999999999999' );
            return ean13( t_val );
          elsif length( trim( p_val ) ) < 13
          then -- check digit not included
            t_val := to_char( to_number( p_val ), 'fm099999999999' );
            t_tmp := 0;
            for i in 1 .. 12
            loop
              t_tmp := t_tmp + ( ascii( substr( t_val, i ) ) - 48 ) * case when mod( i, 2 ) = 1 then 1 else 3 end;
            end loop;
            t_val := t_val || to_char( mod( 10 - mod( t_tmp, 10 ), 10 ), 'fm0' );
            return ean13( t_val );
          end if;
        elsif upper( p_type ) in ( 'CODE39', 'CODE-39' )
        then
          t_val := upper( p_val );
          return code39( translate( t_val
                                  , '01234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ .%$+-/' || t_val
                                  , '01234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ .%$+-/'
                                  )
                       );
        elsif upper( p_type ) in ( 'CODE128', 'CODE-128', 'GS1-128', 'UCC/EAN-128', 'UCC-128', 'EAN-128' )
        then
          if instr( p_parm, '1252' ) > 0
          then
            return code128( utl_i18n.string_to_raw( p_val, 'WE8MSWIN1252' ) );
          end if;
          return code128( utl_i18n.string_to_raw( p_val, 'WE8ISO8859P1' ) );
        elsif upper( p_type ) in ( 'QRCODE', 'QR', 'QR-CODE' )
        then
          return qrcode( p_val, p_parm );
        elsif upper( p_type ) in ( 'AZTEC' )
        then
          return aztec( p_val, p_parm );
        elsif upper( p_type ) in ( 'ITF', 'INTERLEAVED', 'INTERLEAVED2OF5', 'INTERLEAVED-2-OF-5' )
        then
          return itf( p_val, p_parm );
        elsif upper( p_type ) in ( 'EAN2', 'EAN-2' )
        then
          if to_number( p_val ) between 0 and 99
          then
            return ean2( to_char( to_number( p_val ), 'FM09' ) );
          end if;
        elsif upper( p_type ) in ( 'EAN5', 'EAN-5' )
        then
          if to_number( p_val ) between 0 and 99999
          then
            return ean5( to_char( to_number( p_val ), 'FM09999' ) );
          end if;
        end if;
      end if;
    exception
      when others
        then
 dbms_output.put_line( dbms_utility.format_error_stack );dbms_output.put_line( dbms_utility.format_error_backtrace );
          null;
    end;
    return '89504E470D0A1A0A0000000D49484452000000010000000108060000001F15C4890000000D4944415478DA63F8FF9FA11E00077D027EFDBCECEE0000000049454E44AE426082';
  end;
--
  procedure download_barcode( p_val varchar2, p_type varchar2, p_parm varchar2 := null )
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
  function datauri_barcode( p_val varchar2, p_type varchar2, p_parm varchar2 := null )
  return varchar2
  is
  begin
    return 'data:image/png;base64,' || utl_raw.cast_to_varchar2( utl_encode.base64_encode( barcode( p_val, p_type, p_parm ) ) );
  end;
--
  function barcode_blob( p_val varchar2, p_type varchar2, p_parm varchar2 := null )
  return blob
  is
  begin
    return to_blob(barcode(p_val, p_type, p_parm));
  end;
end;
/
