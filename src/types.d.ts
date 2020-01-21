import {
  GraphQLEnumType,
  GraphQLField,
  GraphQLInputType,
  GraphQLOutputType,
  GraphQLResolveInfo,
  GraphQLScalarType
} from 'graphql'
import { ObjectTypeComposer } from 'graphql-compose'
import * as Sequelize from 'sequelize'
import { CacheHint } from  'apollo-cache-control'

export function parseOutpuFields (
  info: GraphQLResolveInfo,
  key?: string
): string[]

export function parseAssociationOutpuFields (
  info: GraphQLResolveInfo,
  key?: string
): string[]

export function getPrimaryKeyField (attributes: any): string

export function getFilterAttributes<TInstance, TAttributes> (
  Model: Sequelize.Model<TInstance, TAttributes>,
  info: GraphQLResolveInfo,
  key: string
): string[]

export function parseQueryOptions (attributes: string[], args: any): any

export interface ArgsPaginationFields {
  page?: number
  paginate?: number
}

export interface PaginationFields {
  limit: number
  offset: number
}

export function parsePagination (args: ArgsPaginationFields): PaginationFields

export function findAll<TInstance, TAttributes> (
  Model: Sequelize.Model<TInstance, TAttributes>,
  args: any,
  info: GraphQLResolveInfo,
  where?: any
): Promise<TInstance[]>

export function createOrderBy (
  rawAttributes: any,
  name: string
): GraphQLEnumType

export function getFields (attributes: string[], args: any): any

export interface QueryResolvers<TInstance> {
  findAll: (
    root: any,
    args: any,
    ctx: any,
    info: GraphQLResolveInfo
  ) => Promise<TInstance[]>
  findOne: (
    root: any,
    args: any,
    ctx: any,
    info: GraphQLResolveInfo
  ) => Promise<TInstance>
}

export function createQueryResolvers<TInstance, TAttributes> (
  Model: Sequelize.Model<TInstance, TAttributes>,
  cacheOptions?: CacheHint
): QueryResolvers<TInstance>

export interface MutationResolvers<TInstance> {
  create: (
    root: any,
    args: any,
    ctx: any,
    info: GraphQLResolveInfo
  ) => Promise<TInstance>
  update: (
    root: any,
    args: any,
    ctx: any,
    info: GraphQLResolveInfo
  ) => Promise<TInstance>
  delete: (
    root: any,
    args: any,
    ctx: any,
    info: GraphQLResolveInfo
  ) => Promise<[TInstance]>
}

export function createMutationResolvers<TInstance, TAttributes> (
  Model: Sequelize.Model<TInstance, TAttributes>
): MutationResolvers<TInstance>

export function sequelizeTypeToGraphQLType (type: string): GraphQLScalarType

export interface FieldOptions {
  allowNull?: boolean
  ignore?: string[]
}

export function modelAttributesToGraphQLFields (
  rawAttributes: any,
  options: FieldOptions
): any

export function attributeToFilters (attribute: any): any

export function createInputQueryFilters (rawAttributes: any): any

export interface BasePaginationField {
  type: GraphQLScalarType
  description: string
}

export interface PaginationField {
  page: BasePaginationField
  paginate: BasePaginationField
}

export function getPaginationFields (): PaginationField & any

export function createQueryArgs (rawAttributes: any, name: string): any

export function modelToType (
  name: string,
  rawAttributes: any,
  options: FieldOptions
): ObjectTypeComposer

export function checkIsModel (target: any): boolean

export interface TypeOptions {
  ignore: string[]
  fields: any
}

export interface ObjectTypeComposerMapper {
  [key: string]: ObjectTypeComposer
}

export function createTypes (
  models: any,
  options?: TypeOptions
): ObjectTypeComposerMapper

export function appendAssociations (
  types: ObjectTypeComposerMapper,
  name: string,
  associations: any
): void
