select InitSpatialMetaDataFull('NONE');

insert into spatial_ref_sys(srid,auth_srid,auth_name,ref_sys_name,proj4text,srtext)
values
 (0x6d6d, 0x6d6d, 'si', 'millimeter relative', 'not implemented', 'not implemented'),
 (0x756d, 0x756d, 'si', 'micrometer relative', 'not implemented', 'not implemented');

create table kicad_text(
       id integer primary key
       , text string not null
       , font_width real not null
       , font_height real not null
       , font_thickness real
       , justify string
);

create table kicad_shape(
       id integer primary key
       , type string
       , exact_geom blob
       , param_geom blob
       , stroke_width real
);


create table kicad_footprint(
       id integer primary key
       , name string
       , layer string
       , tedit string
       , descr string
       , tags_json string
       , attr string
       , version string
       , generator string
);

create table kicad_footprint_draw(
       id integer primary key
       , kicad_footprint_id integer not null
       , layer string
       , d_mm string 
       , width_mm real -- also represents font thickness  
       , text_type string
       , text string
       , font_size_w_mm real
       , font_size_h_mm real
       , text_x_mm real
       , text_y_mm real
       , wkt string
);

create table kicad_pad(
       id integer primary key
       , type string
       , shape string
       , width_mm real
       , height_mm real
       , d_mm string
       , wkt string
       , refcount integer default 1
);

create unique index kicad_pad_unique on kicad_pad(
       type, shape, width_mm, height_mm, d_mm
);

create table kicad_footprint_pad(
       id integer primary key
       , kicad_footprint_id integer not null
       , kicad_pad_id integer not null
       , name string
       , x_mm real
       , y_mm real
       , drill_mm real
       , layers_json string
       , transform string
);

create table svg_atlas(
       def string
       , use string
       , class string
       , transform string
       , text string
       , stroke_width real
       , d string
);

create index svg_atlas_def on svg_atlas(def);
create index svg_atlas_use on svg_atlas(use);

create view svg_atlas_view as
with padshapes as (
select
  printf('kcp%d', kp.id) as def
  , NULL as use
  , NULL as class
--  , printf('pad %s', kp.type) as class
  , NULL as transform
  , NULL as text
  -- , case when shape = 'oval' then min(height_mm, width_mm)
  --        else NULL end as stroke_width
  , NULL as stroke_width
  , d_mm as d
from kicad_pad kp
) , pads as (
select
  printf('kcf%d', fp.id) as def
  , printf('kcp%d', fpp.kicad_pad_id) as use
--  , printf('pad %s %s', kp.type, replace(lj.value, '.', ' ')) as class
  , printf('pad %s', kp.type) as class
  , printf('translate(%f,%f)%s', x_mm, y_mm, fpp.transform) as transform
  , NULL as text
  , NULL as stroke_width
  , NULL as d
from kicad_footprint fp
left join kicad_footprint_pad fpp on fpp.kicad_footprint_id = fp.id
left join kicad_pad kp on kp.id = fpp.kicad_pad_id
-- , json_each(fpp.layers_json) lj 
) , draws as (
select
  printf('kcf%d', fp.id) as def
  , NULL as use
  , printf('%s %s',
           case when width_mm is not null and d.text is null then 'stroked' else null end,
           replace(d.layer, '.', ' ')) as class
  , case when text is not null then printf('translate(%f,%f)', text_x_mm, text_y_mm)
    end as transform
  , d.text as text
  , width_mm as stroke_width
  , group_concat(d_mm, ' ') as d
from kicad_footprint fp
left join kicad_footprint_draw d on fp.id = d.kicad_footprint_id
group by d.text, d.layer, width_mm, fp.id
) , padlabels as (
select
  printf('kcf%d',  fp.id) as def
  , NULL as use
  , 'pad-label' as class
  , printf('translate(%f,%f)%s', x_mm, y_mm, fpp.transform) as transform
  , fpp.name as text
  , NULL as stroke_width
  , NULL as d
from kicad_footprint fp
left join kicad_footprint_pad fpp on fpp.kicad_footprint_id = fp.id
)

select * from padshapes
union all select * from pads
union all select * from draws
--union all select * from padlabels
;
