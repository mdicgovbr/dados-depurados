-- Table: public."UF-Br"

-- DROP TABLE public."UF-Br";

CREATE TABLE public."UF-Br"
(
  sigla character varying NOT NULL,
  nome character varying,
  CONSTRAINT "UF-Br_pkey" PRIMARY KEY (sigla)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE public."UF-Br"
  OWNER TO postgres;
