relocant
===

A PostgreSQL migration CLI tool and library.

Synopsis
---

  - The user tells _relocant_ that there is a directory containing migration scripts;
  - _relocant_ (advisory-)locks the database;
  - _relocant_ figures out which scripts haven't yet been applied;
  - _relocant_ sorts unapplied scripts lexicographically, creating a migration plan;
  - _relocant_ runs each script in a separate transaction;
  - _relocant_ either records a successfully applied migration or aborts the migration plan on a failure;
  - _relocant_ (advisory-)unlocks the database.

CLI Workflow
---

The _relocant_ CLI tool attempts to provide a simple and reliable way to
apply migration scripts and track them. A migration script is any file with an `sql` extension.
The longest alpha-numeric prefix of the migration script is its ID. The tool requires all migration
scripts to reside inside a single directory.

> [!NOTE]
> The path to the scripts directory can be configured by
>
>   - either setting the `RELOCANT_SCRIPTS_DIR` environment variable,
>   - or providing the path to the `--directory` option

If we run `relocant list-unapplied` we'll get back a migration plan, listing those scripts
that haven't yet been recorded as applied in the database, in the order they will be applied:

```shell
% ls ./migration
001-initial-schema.sql
002-description.sql
% relocant list-unapplied --directory ./migration
001     001-initial-schema      dfde7438
002     002-description         74f8a76e
```

Now, if we like this plan, we can run `relocant apply` to actually apply the scripts to our database.

> [!IMPORTANT]
> Each script is run in a separate transaction and recorded as part of it.
> Unfortunately, this means that there are restrictions on what you can put
> in a migration script, including these:
>
>   1. Using `BEGIN`, `ROLLBACK`, and `COMMIT` will likely break _relocant_.
>   2. Running `CREATE INDEX CONCURRENTLY` will cause the migration script to fail immediately.

> [!NOTE]
> Successfully running _relocant_ commands needing access to the DB would require
>
>   - either setting `PG*` variables up,
>   - or providing a connection string to the `--connection-string` option
>
> Here, I'm running _relocant_ inside a _nix-shell_ where the environment has been already set up
> correctly.

```shell
% relocant apply --directory ./migration
001     001-initial-schema      dfde7438
001     001-initial-schema      dfde7438        2024-10-24 15:04:09 +0000       0.01
002     002-description         74f8a76e
002     002-description         74f8a76e        2024-10-24 15:04:09 +0000       0.01
```

By running `relocant list-applied`, we can check that the applied scripts have been recorded correctly.
Naturally, the recorded migrations will be sorted in the order of application:

```shell
% relocant list-applied
001     001-initial-schema      dfde7438        2024-10-24 15:04:09 +0000       0.01s
002     002-description         74f8a76e        2024-10-24 15:04:09 +0000       0.00s
```

Once we have another migration script to apply, we put it in the `migration` directory and re-run `relocant apply`:

```shell
% relocant list-unapplied --directory ./migration
003     003-fix-typo            a7032e4f
% relocant apply
003     003-fix-typo            a3582319
003     003-fix-typo            a3582319        2024-10-24 15:24:09 +0000       0.00s
% relocant list-applied --directory ./migration
001     001-initial-schema      dfde7438        2024-10-24 15:04:09 +0000       0.01s
002     002-description         74f8a76e        2024-10-24 15:04:09 +0000       0.00s
003     003-fix-typo            a3582319        2024-10-24 15:24:09 +0000       0.00s
```

Acknowledgements
---

I've been using a (very) similar workflow for a while now and it worked great for me, but I wouldn't have
bothered to make it a standalone tool if I haven't stumbled upon [_pgmigrate_][0] by Peter Downs.

  [0]: https://github.com/peterldowns/pgmigrate
