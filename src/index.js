'use strict'

const {
  GraphQLBoolean,
  GraphQLEnumType,
  GraphQLFloat,
  GraphQLInt,
  GraphQLList,
  GraphQLNonNull,
  GraphQLObjectType,
  GraphQLString
} = require('graphql/type')
const { schemaComposer, GraphQLDate } = require('graphql-compose')
const graphqlFields = require('graphql-fields')
const sequelize = require('sequelize')

/**
 * @typedef {import('graphql').GraphQLResolveInfo} GraphQLResolveInfo
 */
/**
 * Get output fields from GraphQL AST
 * @param {GraphQLResolveInfo} info - GraphQL info resolver arg
 * @param {string} [key=null] - Optional key for change level
 * @returns {Array<string>} - List of fields
 */
const parseOutpuFields = (info, key = null) => {
  let outputFields = graphqlFields(info)
  if (key) outputFields = outputFields[key]
  return Object.keys(outputFields).reduce((fields, field) => {
    if (Object.keys(outputFields[field]).length === 0) {
      fields.push(field)
    }
    return fields
  }, [])
}

/**
 * Get output fields from GraphQL AST
 * @param {GraphQLResolveInfo} info - GraphQL info resolver arg
 * @param {string} [key=null] - Optional key for change level
 * @returns {Array<string>} - List of fields
 */
const parseAssociationOutpuFields = (info, key = null) => {
  let outputFields = graphqlFields(info)
  if (key) outputFields = outputFields[key]
  return Object.keys(outputFields).reduce((fields, field) => {
    if (Object.keys(outputFields[field]).length > 0) {
      fields.push(field)
    }
    return fields
  }, [])
}

/**
 * Get primary key from Sequelize model attributes
 * @param {Object} attributes Sequelize model attributes
 * @returns {string|null} Primary key field name
 */
const getPrimaryKeyField = attributes => {
  return Object.keys(attributes).reduce((primaryKey, field) => {
    if (attributes[field].primaryKey === true) {
      primaryKey = field
    }
    return primaryKey
  }, null)
}

/**
 * Get attributes selected from info resolve argument
 * @param {Object} Model
 * @param {GraphQLResolveInfo} info GraphQL info resolver arg
 * @param {string} [key=null] - Optional key for change level
 * @returns {Array<string>} List of attributes
 */
const getFilterAttributes = (Model, info, key = null) => {
  const primaryKeyField = getPrimaryKeyField(Model.rawAttributes)
  const attributes = parseOutpuFields(info, key)
  if (!attributes.includes(primaryKeyField)) attributes.push(primaryKeyField)
  const associationAttributes = parseAssociationOutpuFields(info, key)
  const associationAttributesMatches = Object.entries(
    Model.associations
  ).reduce((acc, [key, association]) => {
    if (
      associationAttributes.includes(key) &&
      association.associationType === 'BelongsTo'
    ) {
      acc.push(association.foreignKey)
    }
    return acc
  }, [])
  return [...new Set([...attributes, ...associationAttributesMatches])]
}

/**
 * Convert input args to Sequelize query options
 * @param {Array<string>} attributes List of model attributes name
 * @param {Object} args Args from revolver
 * @returns {Object} Where filter
 */
const parseQueryOptions = (attributes, args) => {
  const options = {}
  const where = attributes.reduce((acc, field) => {
    if (args.filter) {
      if (typeof args.filter.OR !== 'undefined') {
        acc[sequelize.Op.or] = parseQueryOptions(
          attributes,
          args.filter.OR.reduce(
            (acc, curr) => {
              Object.assign(acc.filter, curr)
              return acc
            },
            { filter: {} }
          )
        ).where
      } else if (typeof args.filter.AND !== 'undefined') {
        acc[sequelize.Op.and] = parseQueryOptions(
          attributes,
          args.filter.AND.reduce(
            (acc, curr) => {
              Object.assign(acc.filter, curr)
              return acc
            },
            { filter: {} }
          )
        ).where
      } else {
        if (typeof args.filter[field] !== 'undefined') {
          if (!acc[field]) acc[field] = {}
          acc[field][sequelize.Op.eq] = args.filter[field]
        }
        if (typeof args.filter[`${field}_not`] !== 'undefined') {
          if (!acc[field]) acc[field] = {}
          acc[field][sequelize.Op.ne] = args.filter[`${field}_not`]
        }
        if (typeof args.filter[`${field}_in`] !== 'undefined') {
          if (!acc[field]) acc[field] = {}
          acc[field][sequelize.Op.in] = args.filter[`${field}_in`]
        }
        if (typeof args.filter[`${field}_nin`] !== 'undefined') {
          if (!acc[field]) acc[field] = {}
          acc[field][sequelize.Op.notIn] = args.filter[`${field}_nin`]
        }
        if (typeof args.filter[`${field}_lt`] !== 'undefined') {
          if (!acc[field]) acc[field] = {}
          acc[field][sequelize.Op.lt] = args.filter[`${field}_lt`]
        }
        if (typeof args.filter[`${field}_gt`] !== 'undefined') {
          if (!acc[field]) acc[field] = {}
          acc[field][sequelize.Op.gt] = args.filter[`${field}_gt`]
        }
        if (typeof args.filter[`${field}_lte`] !== 'undefined') {
          if (!acc[field]) acc[field] = {}
          acc[field][sequelize.Op.lte] = args.filter[`${field}_lte`]
        }
        if (typeof args.filter[`${field}_gte`] !== 'undefined') {
          if (!acc[field]) acc[field] = {}
          acc[field][sequelize.Op.gte] = args.filter[`${field}_gte`]
        }
        if (typeof args.filter[`${field}_contains`] !== 'undefined') {
          if (!acc[field]) acc[field] = {}
          acc[field][sequelize.Op.like] =
            '%' + args.filter[`${field}_contains`] + '%'
        }
        if (typeof args.filter[`${field}_not_contains`] !== 'undefined') {
          if (!acc[field]) acc[field] = {}
          acc[field][sequelize.Op.notLike] =
            '%' + args.filter[`${field}_not_contains`] + '%'
        }
        if (typeof args.filter[`${field}_starts_with`] !== 'undefined') {
          if (!acc[field]) acc[field] = {}
          acc[field][sequelize.Op.like] =
            args.filter[`${field}_starts_with`] + '%'
        }
        if (typeof args.filter[`${field}_not_starts_with`] !== 'undefined') {
          if (!acc[field]) acc[field] = {}
          acc[field][sequelize.Op.notLike] =
            args.filter[`${field}_not_starts_with`] + '%'
        }
        if (typeof args.filter[`${field}_ends_with`] !== 'undefined') {
          if (!acc[field]) acc[field] = {}
          acc[field][sequelize.Op.like] =
            '%' + args.filter[`${field}_ends_with`]
        }
        if (typeof args.filter[`${field}_not_ends_with`] !== 'undefined') {
          if (!acc[field]) acc[field] = {}
          acc[field][sequelize.Op.notLike] =
            '%' + args.filter[`${field}_not_ends_with`]
        }
      }
    }
    return acc
  }, {})
  if (args.orderBy) {
    const match = /^(\w+)_(ASC|DESC)$/.exec(args.orderBy)
    if (match.length === 3 && attributes.includes(match[1])) {
      options.order = [[match[1], match[2]]]
    }
  }
  options.where = where
  return options
}

/**
 * @typedef {Object} ArgsPaginationFields
 * @property {number} [page]
 * @property {number} [paginate]
 */
/**
 * @typedef {Object} PaginationFields
 * @property {number} limit
 * @property {number} offset
 */
/**
 * Parse paginate fields from args
 * @param {ArgsPaginationFields} args Args from revolver
 * @returns {PaginationFields}
 */
const parsePagination = args => {
  const page = args.page || 1
  const paginate = args.paginate || 25
  const limit = paginate
  const offset = paginate * (page - 1)
  return { limit, offset }
}

/**
 * Get filter docs from a model
 * @param {Object} Model -
 * @param {Object} args Args from revolver
 * @param {GraphQLResolveInfo} info - GraphQL info resolver arg
 * @param {Object} [where={}] Extra query filter
 * @returns {Promise<Array<Object>>} List of docs
 */
const findAll = async (Model, args, info, where = {}) => {
  const options = parseQueryOptions(Object.keys(Model.rawAttributes), args)
  const { limit, offset } = parsePagination(args)
  options.limit = limit
  options.offset = offset
  options.attributes = getFilterAttributes(Model, info)
  Object.keys(where).forEach(key => {
    if (typeof options.where[key] === 'undefined') {
      options.where[key] = where[key]
    } else {
      Object.assign(options.where[key], where[key])
    }
  })
  const docs = await Model.findAll(options)
  return docs
}

/**
 * @typedef {import('graphql').GraphQLEnumType} GraphQLEnumType
 */
/**
 * Create orderBy EnumType
 * @param {Object} rawAttributes Sequelize model attributes
 * @param {string} name
 * @returns {GraphQLEnumType}
 */
const createOrderBy = (rawAttributes, name) => {
  const values = Object.keys(rawAttributes).reduce((acc, field) => {
    acc[`${field}_ASC`] = { value: `${field}_ASC` }
    acc[`${field}_DESC`] = { value: `${field}_DESC` }
    return acc
  }, {})
  return new GraphQLEnumType({
    name: `${name}OrderBy`,
    description: 'Order by any model field',
    values: values
  })
}

/**
 * Get models attributes from graphql input args
 * @param {Array<string>} attributes List of model attributes name
 * @param {Object} args Args from revolver
 * @returns {Object} Model attributes with values
 */
const getFields = (attributes, args) => {
  return attributes.reduce((acc, field) => {
    if (typeof args[field] !== 'undefined') {
      acc[field] = args[field]
    }
    return acc
  }, {})
}

/**
 * Create generic query resolvers (findAll and findOne)
 * @param {Object} Model
 * @returns {Object} Object with findAll and findOne resolvers
 */
const createQueryResolvers = Model => {
  return {
    findAll: async (root, args, ctx, info) => {
      const docs = await findAll(Model, args, info)
      return docs
    },
    findOne: async (root, args, ctx, info) => {
      const options = {
        where: {
          id: args.id
        }
      }
      options.attributes = getFilterAttributes(Model, info)
      const doc = await Model.findOne(options)
      return doc
    }
  }
}

/**
 * Create generic mutation resolvers (create, update y delete)
 * @param {Object} Model
 * @returns {Object} Object with create, update y delete resolvers
 */
const createMutationResolvers = Model => {
  return {
    create: async (root, args) => {
      const data = getFields(Object.keys(Model.rawAttributes), args)
      const doc = await Model.create(data)
      return doc
    },
    update: async (root, args) => {
      const doc = await Model.findOne({
        where: {
          id: args.id
        }
      })
      const data = getFields(Object.keys(Model.rawAttributes), args)
      await doc.update(data)
      return doc
    },
    delete: async (root, args) => {
      const doc = await Model.findOne({
        where: {
          id: args.id
        }
      })
      const result = await Model.softDelete({
        where: {
          id: args.id
        }
      })
      return result > 0 ? doc : null
    }
  }
}

/**
 * @typedef {import('graphql').GraphQLScalarType} GraphQLScalarType
 */
/**
 * Convert Sequelize attibute type to GraphQL type
 * @param {string} type
 * @returns {GraphQLScalarType} GraphQLScalarType
 */
const sequelizeTypeToGraphQLType = type => {
  const attributes = new Map([
    ['BOOLEAN', GraphQLBoolean],
    ['FLOAT', GraphQLFloat],
    ['DOUBLE', GraphQLFloat],
    ['DOUBLE PRECISION', GraphQLFloat],
    ['INTEGER', GraphQLInt],
    ['CHAR', GraphQLString],
    ['STRING', GraphQLString],
    ['TEXT', GraphQLString],
    ['UUID', GraphQLString],
    ['DATE', GraphQLDate],
    ['DATEONLY', GraphQLDate],
    ['TIME', GraphQLString],
    ['BIGINT', GraphQLString],
    ['DECIMAL', GraphQLString],
    ['VIRTUAL', GraphQLString]
  ])
  return attributes.get(type)
}

/**
 * @typedef {Object} FieldOptions
 * @property {Boolean} [allowNull=false] Remove GraphQLNonNull constraint
 * @property {Array<string>} [ignore=[]] List of fields to ignore
 */
/**
 * Convert model attributes to graphql input fields
 * @param {Object} rawAttributes Sequelize model attributes
 * @param {FieldOptions} [options] Fields options
 * @returns {Object}
 */
const modelAttributesToGraphQLFields = (
  rawAttributes,
  options = { allowNull: false, ignore: [] }
) => {
  const { allowNull, ignore } = options
  return Object.keys(rawAttributes).reduce((acc, key) => {
    if (!ignore.includes(key)) {
      const attribute = rawAttributes[key]
      const type = attribute.type
      acc[key] = {
        type: sequelizeTypeToGraphQLType(type.key)
      }
      if (!allowNull) {
        if (attribute.allowNull === false || attribute.primaryKey === true) {
          acc[key].type = new GraphQLNonNull(acc[key].type)
        }
      }
      if (typeof attribute.comment === 'string') {
        acc[key].description = attribute.comment
      }
    }
    return acc
  }, {})
}

/**
 * @typedef {Object} SequelizeAttributeType
 * @property {string} key
 */
/**
 * @typedef {Object} SequelizeAttribute
 * @property {string} fieldName
 * @property {SequelizeAttributeType} type
 */
/**
 * Create input query filters from model attribute
 * @param {SequelizeAttribute} attribute Sequelize model attribute
 * @returns {Object} Object with query filters
 */
const attributeToFilters = attribute => {
  const booleanFilters = {
    [attribute.fieldName]: {
      type: GraphQLBoolean
    },
    [`${attribute.fieldName}_not`]: {
      type: GraphQLBoolean,
      description: 'All values that are not equal to given value.'
    }
  }
  const numberFilters = {
    [attribute.fieldName]: {
      type: GraphQLInt
    },
    [`${attribute.fieldName}_not`]: {
      type: GraphQLInt,
      description: 'All values that are not equal to given value.'
    },
    [`${attribute.fieldName}_in`]: {
      type: new GraphQLList(GraphQLInt),
      description: 'All values that are contained in given list.'
    },
    [`${attribute.fieldName}_not_in`]: {
      type: new GraphQLList(GraphQLInt),
      description: 'All values that are not contained in given list.'
    },
    [`${attribute.fieldName}_lt`]: {
      type: GraphQLInt,
      description: 'All values less than the given value.'
    },
    [`${attribute.fieldName}_gt`]: {
      type: GraphQLInt,
      description: 'All values greater than the given value.'
    },
    [`${attribute.fieldName}_lte`]: {
      type: GraphQLInt,
      description: 'All values less than or equal the given value.'
    },
    [`${attribute.fieldName}_gte`]: {
      type: GraphQLInt,
      description: 'All values greater than or equal the given value.'
    }
  }
  const stringFilters = {
    [attribute.fieldName]: {
      type: GraphQLString
    },
    [`${attribute.fieldName}_not`]: {
      type: GraphQLString,
      description: 'All values that are not equal to given value.'
    },
    [`${attribute.fieldName}_in`]: {
      type: new GraphQLList(GraphQLString),
      description: 'All values that are contained in given list.'
    },
    [`${attribute.fieldName}_not_in`]: {
      type: new GraphQLList(GraphQLString),
      description: 'All values that are not contained in given list.'
    },
    [`${attribute.fieldName}_contains`]: {
      type: GraphQLString,
      description: 'All values containing the given string.'
    },
    [`${attribute.fieldName}_not_contains`]: {
      type: GraphQLString,
      description: 'All values not containing the given string.'
    },
    [`${attribute.fieldName}_starts_with`]: {
      type: GraphQLString,
      description: 'All values starting with the given string.'
    },
    [`${attribute.fieldName}_ends_with`]: {
      type: GraphQLString,
      description: 'All values ending with the given string.'
    },
    [`${attribute.fieldName}_not_starts_with`]: {
      type: GraphQLString,
      description: 'All values not starting with the given string.'
    },
    [`${attribute.fieldName}_not_ends_with`]: {
      type: GraphQLString,
      description: 'All values not ending with the given string.'
    }
  }
  const attributes = new Map([
    ['BOOLEAN', booleanFilters],
    ['FLOAT', numberFilters],
    ['DOUBLE', numberFilters],
    ['DOUBLE PRECISION', numberFilters],
    ['INTEGER', numberFilters],
    ['CHAR', stringFilters],
    ['STRING', stringFilters],
    ['TEXT', stringFilters],
    ['UUID', stringFilters],
    ['DATE', stringFilters],
    ['DATEONLY', stringFilters],
    ['TIME', stringFilters],
    ['BIGINT', stringFilters],
    ['DECIMAL', stringFilters]
  ])
  return attributes.get(attribute.type.key)
}

/**
 * Create input query filters from all model attributes
 * @param {Object} attributes Sequelize model attributes
 * @returns {Object} Input query fields
 */
const createInputQueryFilters = attributes => {
  return Object.keys(attributes).reduce((fields, attibute) => {
    const attribute = attributes[attibute]
    Object.assign(fields, attributeToFilters(attribute))
    return fields
  }, {})
}

/**
 * @typedef {Object} BasePaginationField
 * @property {import('graphql').GraphQLScalarType} type
 * @property {string} description
 */
/**
 * @typedef {Object} PaginationField
 * @property {BasePaginationField} page
 * @property {BasePaginationField} paginate
 */
/**
 * Pagination fields to input query
 * @returns {PaginationField}
 */
const getPaginationFields = () => {
  return {
    page: {
      type: GraphQLInt,
      description: 'Set number page for pagination'
    },
    paginate: {
      type: GraphQLInt,
      description: 'Set number of elements per page for pagination'
    }
  }
}

/**
 * Create generic input args (filter, orderBy, page, paginate)
 * @param {Object} attributes Sequelize model attributes
 * @param {string} name Name for filter and order input types
 * @returns {Object} Object with filter, orderBy, page, paginate
 */
const createQueryArgs = (attributes, name) => {
  const filter = schemaComposer.createInputTC({
    name: `${name}Filter`,
    fields: createInputQueryFilters(attributes)
  })
  filter.addFields({
    OR: [filter],
    AND: [filter]
  })
  return {
    filter: {
      type: filter.getType(),
      description: 'Filter query parameters'
    },
    orderBy: {
      type: createOrderBy(attributes, name),
      description: 'Set order by any model attribute'
    },
    ...getPaginationFields()
  }
}

/**
 * @typedef {import('graphql-compose').ObjectTypeComposer} ObjectTypeComposer
 */
/**
 * Create GraphQL Type from Sequelize Model
 * @param {string} name Model name
 * @param {Object} rawAttributes Sequelize model attributes
 * @param {FieldOptions} options Field options
 * @returns {ObjectTypeComposer}
 */
const modelToType = (name, rawAttributes, options) => {
  return schemaComposer.createObjectTC(
    new GraphQLObjectType({
      name: name,
      fields: modelAttributesToGraphQLFields(rawAttributes, options)
    })
  )
}

/**
 * @param {Object} target -
 */
const checkIsModel = target => !!target.getTableName

/**
 * @typedef TypeOptions
 * @property {Array<string>} [ignore=[]] List of models name to ignore
 * @property {Object} [fields={}] Field options by Model
 */
/**
 * Create all GraphQL Type from a list of Sequelize Model
 * @param {Object} models Sequelize instance with models
 * @param {TypeOptions} options Type options
 * @returns {Object} Object with all GraphQL types from models
 */
const createTypes = (models, options = { ignore: [], fields: {} }) => {
  const { ignore, fields } = options
  return Object.keys(models).reduce((types, name) => {
    if (checkIsModel(models[name]) && !ignore.includes(name)) {
      types[name] = modelToType(
        models[name].name,
        models[name].rawAttributes,
        fields[name]
      )
    }
    return types
  }, {})
}

/**
 * Add model relation fields
 * @param {Object} types All graphql types from models
 * @param {string} name Model Type name
 * @param {Object} associations Model relations
 */
const appendAssociations = (types, name, associations) => {
  const _associations = Object.entries(associations)
  if (_associations.length > 0) {
    const associationFields = _associations.reduce(
      (fields, [key, association]) => {
        try {
          if (association.associationType === 'HasMany') {
            fields[key] = {
              type: new GraphQLList(types[association.target.name].getType()),
              args: createQueryArgs(
                association.target.rawAttributes,
                `${association.source.name}${key}`
              ),
              resolve: async (parent, args, ctx, info) => {
                const options = parseQueryOptions(
                  Object.keys(association.target.rawAttributes),
                  args
                )
                const { limit, offset } = parsePagination(args)
                options.limit = limit
                options.offset = offset
                options.attributes = getFilterAttributes(
                  association.target,
                  info
                )
                if (!options.attributes.includes(association.foreignKey)) {
                  options.attributes.push(association.foreignKey)
                }
                const docs = await parent[association.accessors.get](options)
                return docs
              }
            }
          } else if (association.associationType === 'BelongsTo') {
            fields[key] = {
              type: types[association.target.name].getType(),
              args: createQueryArgs(
                association.target.rawAttributes,
                `${association.source.name}${key}`
              ),
              resolve: async (parent, args, ctx, info) => {
                const options = {}
                options.attributes = getFilterAttributes(
                  association.target,
                  info
                )
                if (association.options.targetKey) {
                  options.attributes.push(association.options.targetKey)
                }
                const docs = await parent[association.accessors.get](options)
                return docs
              }
            }
          } else if (association.associationType === 'BelongsToMany') {
            fields[key] = {
              type: new GraphQLList(types[association.target.name].getType()),
              args: createQueryArgs(
                association.target.rawAttributes,
                `${association.source.name}${key}`
              ),
              resolve: async (parent, args, ctx, info) => {
                const options = parseQueryOptions(
                  Object.keys(association.target.rawAttributes),
                  args
                )
                const { limit, offset } = parsePagination(args)
                options.limit = limit
                options.offset = offset
                options.attributes = getFilterAttributes(
                  association.target,
                  info
                )
                const docs = await parent[association.accessors.get](options)
                return docs
              }
            }
          } else if (association.associationType === 'HasOne') {
            fields[key] = {
              type: types[association.target.name].getType(),
              args: createQueryArgs(
                association.target.rawAttributes,
                `${association.source.name}${key}`
              ),
              resolve: async (parent, args, ctx, info) => {
                const options = {}
                options.attributes = getFilterAttributes(
                  association.target,
                  info
                )
                if (!options.attributes.includes(association.foreignKey)) {
                  options.attributes.push(association.foreignKey)
                }
                const docs = await parent[association.accessors.get](options)
                return docs
              }
            }
          }
        } catch (err) {}
        return fields
      },
      {}
    )
    types[name].addFields(associationFields)
  }
}

module.exports = {
  parseOutpuFields,
  getPrimaryKeyField,
  getFilterAttributes,
  findAll,
  parseQueryOptions,
  createOrderBy,
  getFields,
  createQueryResolvers,
  createMutationResolvers,
  sequelizeTypeToGraphQLType,
  modelAttributesToGraphQLFields,
  attributeToFilters,
  createInputQueryFilters,
  getPaginationFields,
  createQueryArgs,
  createTypes,
  appendAssociations
}
