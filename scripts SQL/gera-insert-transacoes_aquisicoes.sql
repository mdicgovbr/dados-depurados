Select 
    'INSERT INTO TRANSACOES (valor, siglaUF, idPais, nome_pais, nbs, ano, modo) VALUES (''' ||
    valor || ''',''' ||
    uf_origem || ''',''' ||
    codigo_país || ''',''' ||
    NBS || ''', ''2017'', ''aquisicao'')'

FROM(
    select 
    sum(OP.VALOROPERACAOUSD) as valor, 
    RAS.UF as uf_origem, 
    RAS.CODIGOPAISVENDEDOR as codigo_país, 
    pai.nome as nome_país,
    OP.CODIGONBS as NBS
    
    FROM SISCOSERVA201804.DADOSOPERACOES OP
    JOIN SISCOSERVA201804.DADOSRAS RAS on RAS.ID_RAS = OP.ID_RAS
    JOIN SISCOSERVA201804.PAISES pai on pai.CODIGO = RAS.CODIGOPAISVENDEDOR

-- Provisório enquanto desenvolvemos    
    WHERE rownum <= 100
    
    GROUP BY RAS.UF, RAS.CODIGOPAISVENDEDOR, OP.CODIGONBS, pai.nome
);

