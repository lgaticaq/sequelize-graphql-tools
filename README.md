# sequelize-graphql-tools

[![npm version](https://img.shields.io/npm/v/@eclass/sequelize-graphql-tools.svg)](https://www.npmjs.com/package/@eclass/sequelize-graphql-tools)
[![npm downloads](https://img.shields.io/npm/dm/@eclass/sequelize-graphql-tools.svg)](https://www.npmjs.com/package/@eclass/sequelize-graphql-tools)
[![Maintainability](https://api.codeclimate.com/v1/badges/7d2accb39a80b8ee6573/maintainability)](https://codeclimate.com/github/eclass/sequelize-graphql-tools/maintainability)
[![devDependency Status](https://img.shields.io/david/dev/eclass/sequelize-graphql-tools.svg)](https://david-dm.org/eclass/sequelize-graphql-tools#info=devDependencies)

> Utils to generate graphql schema, types, querys and mutations from sequelize models

## Installation

```bash
npm i sequelize-graphql-tools graphql graphql-compose graphql-fields graphql-iso-date sequelize
```

## Use

### One model to GraphqlType

```js
const { modelToType } = require('@eclass/sequelize-graphql-tools')
const db = require('./models') // Sequelize instance with all models imported

const options = {
  ignore: ['password'] // Hide password in fields
}
const UserTC = modelToType(db.User.name, db.User.rawAttributes, options)
```

### All model to GraphqlType

```js
const { createTypes } = require('@eclass/sequelize-graphql-tools')
const db = require('./models') // Sequelize instance with all models imported

const options = {
  ignore: ['Session'], // Ignore models
  fields: {
    User: { ignore: ['password'] } // Hide password in User fields
  }
}
const allTypes = createTypes(db, options)
const UserTC = allTypes.User
```

### Append associations

```js
const { createTypes, appendAssociations } = require('@eclass/sequelize-graphql-tools')
const db = require('./models') // Sequelize instance with all models imported

const options = {
  ignore: ['Session'], // Ignore models
  fields: {
    User: { ignore: ['password'] } // Hide password in User fields
  }
}
const allTypes = createTypes(db, options)
Object.keys(allTypes).forEach(name => {
  appendAssociations(allTypes, name, db[name].associations)
})
```

### Generate all querys and mutations

```js
const { GraphQLSchema, GraphQLObjectType } = require('graphql/type')
const { createTypes, appendAssociations } = require('@eclass/sequelize-graphql-tools')
const db = require('./models') // Sequelize instance with all models imported

const options = {
  ignore: ['Session'], // Ignore models
  fields: {
    User: { ignore: ['password'] } // Hide password in User fields
  }
}
const allTypes = createTypes(db, options)
Object.keys(allTypes).forEach(name => {
  appendAssociations(allTypes, name, db[name].associations)
})
const querys = Object.keys(allTypes).reduce((acc, name) => {
  return { ...acc, ...createQuery(db[name], allTypes[name].gqType) }
}, {})
const mutations = Object.keys(allTypes).reduce((acc, name) => {
  return {
    ...acc,
    ...createMutation(db[name], allTypes[name].gqType)
  }
}, {})
const query = new GraphQLObjectType({
  name: 'query',
  fields: () => ({ ...querys })
})
const mutation = new GraphQLObjectType({
  name: 'mutation',
  fields: () => ({ ...mutations })
})
const schema = new GraphQLSchema({ query, mutation })
```

## Licencia

[MIT](https://tldrlegal.com/license/mit-license)
