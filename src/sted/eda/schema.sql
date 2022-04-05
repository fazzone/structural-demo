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
);

create table specctra_dsn(
       id integer primary key
       , pcb_id string
       , host_cad string
       , host_version string
       , resolution_unit string
       , resolution real
       , unit string

       , pcb_boundary_shape blob
);

create table dsn_layer(
       id integer primary key
       , dsn_id integer
       , type string
       , idx integer
);

create table dsn_plane(
       id integer primary key
       , dsn_id integer
       , name string
       , aperture_width real
       , dsn_layer_id integer
       , plane_shape blob
);

create table specctra_window(
       id integer primary key
       , dsn_id integer
       , dsn_plane_id integer
       , window_shape blob
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
  , printf('pad %s', kp.type) as class
  , NULL as transform
  , NULL as text
  , case when shape = 'oval' then width_mm
         else NULL end as stroke_width
  , d_mm as d
from kicad_pad kp
) , pads as (
select
  printf('kcf%d', fp.id) as def
  , printf('kcp%d', fpp.kicad_pad_id) as use
  , NULL as class
  , printf('translate(%f,%f)', x_mm, y_mm) as transform
  , NULL as text
  , NULL as stroke_width
  , NULL as d
from kicad_footprint fp
left join kicad_footprint_pad fpp on fpp.kicad_footprint_id = fp.id
) , draws as (
select
  printf('kcf%d', fp.id) as def
  , NULL as use
  , d.layer as class
  , NULL as transform
  , d.text as text
  , width_mm as stroke_width
  , group_concat(d_mm, ' ') as d
from kicad_footprint fp
left join kicad_footprint_draw d on fp.id = d.kicad_footprint_id
group by d.text, d.layer, width_mm, fp.id
)

select * from padshapes
union all select * from pads
union all select * from draws
;
