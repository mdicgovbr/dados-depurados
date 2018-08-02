-- Table: public.transacoes

-- DROP TABLE public.transacoes;

CREATE TABLE public.transacoes
(
  id serial primary key,
  "Valor" double precision,
  "siglaUF" character varying,
  "NBS" character varying,
  "Ano" integer,
  "Modo" character varying,
  "idPais" integer,
  CONSTRAINT "transacoes_idPais_fkey" FOREIGN KEY ("idPais")
      REFERENCES public.pais ("ID") MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION,
  CONSTRAINT "transacoes_siglaUF_fkey" FOREIGN KEY ("siglaUF")
      REFERENCES public."UF-Br" (sigla) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION
)
WITH (
  OIDS=FALSE
);
ALTER TABLE public.transacoes
  OWNER TO postgres;
