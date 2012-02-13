/**
 * В этом файле будем держать описание индексов,
 * (для ускорения базы данных)
 * если они нам вообще потребуются
**/

-- create index attach2doc_doc_id_idx on attach2doc(doc_id);
-- create index directory_parent_dir_id_idx on directory(parent_dir_id);
-- create index directory_doc_description_id_idx on directory(doc_description_id);
-- create index  document_dir_id_idx on document(dir_id);

create index acv_video_url_idx on acv_video(url);
