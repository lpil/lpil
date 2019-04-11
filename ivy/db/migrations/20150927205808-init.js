'use strict';

module.exports = {
  up: (migration, Type) => {
    return migration.createTable(
      'faqs',
      {
        id: {
          type: Type.INTEGER,
          primaryKey: true,
          autoIncrement: true,
        },
        createdAt: {
          type: Type.DATE,
          allowNull: false,
        },
        updatedAt: {
          type: Type.DATE,
          allowNull: false,
        },
        question: {
          type: Type.TEXT,
          allowNull: false,
        },
        answer: {
          type: Type.TEXT,
          allowNull: false,
        },
      }
    );
  },

  down: (migration, type) => {
    return migration.dropAllTables();
  },
};
