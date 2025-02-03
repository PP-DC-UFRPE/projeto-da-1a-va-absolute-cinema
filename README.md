# 🎬 Absolute Cinema - Sistema de Gestão de Cinemas em Haskell

Este sistema tem como objetivo fornecer funcionalidades para a gestão de sessões de filmes em uma plataforma de venda de ingressos online para um cinema.

## 🛠️ Tecnologias Utilizadas
- **IORef**: Gerenciamento de estado seguro
- **Sistema Modular**: Separação clara de responsabilidades
- **Type Safety**: Tipos customizados para validação
- **CLI Interativo**: Interface amigável via terminal

## Recursos Principais

### 🎟️ Para Usuários
- **Consulta de Sessões**
  - Listagem de filmes com horários, salas e detalhes
  - Visualização de sinopses, gêneros e durações
- **Sistema de Compra de ingressos**
  - Listagem de filmes com horários, salas e detalhes disponiveis
  - Compra de ingressos, sendo eles inteira ou meia
- **Sistema de Cadastro**
  - Registro de novos clientes

### 🔐 Painel Administrativo (Modo Admin)
- **Gestão de Conteúdo**
  - CRUD completo de filmes
  - Gerenciamento de sessões (horários/salas)
- **Controle de Clientes**
  - Listagem de usuários cadastrados
  - Ferramentas de moderação
- **Operações Seguras**
  - Acesso protegido por senha
  - Persistência de dados em arquivo

## 🚀 Como Executar

### Pré-requisitos
- GHC 8.10+
- Git

### Execução
```bash
git clone https://github.com/seu-usuario/absolute-cinema.git
cd absolute-cinema
ghci :load Main.hs - Carregar arquivos
ghci main - Executar programa
```