-- Table: public.pais

-- DROP TABLE public.pais;

CREATE TABLE public.pais
(
  nome character varying,
  "ID" integer NOT NULL,
  CONSTRAINT pais_pkey PRIMARY KEY ("ID")
)
WITH (
  OIDS=FALSE
);
ALTER TABLE public.pais
  OWNER TO postgres;
