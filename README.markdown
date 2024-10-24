relocant
===

A PostgreSQL migration CLI tool and library.

Workflow
---

The _relocant_ CLI tool attempts to provide a simple and reliable way to
apply migration scripts and track them. A migration script is any file with an `sql` extension.
The longest alpha-numeric prefix of the migration script is its ID. The tool requires all migration
scripts to reside inside a single directory, for example:

```shell
% ls ./migration
001-initial-schema.sql
002-description.sql
```

If we run `relocant list-unapplied` we'll get back a migration plan, listing those scripts
that haven't yet been recorded as applied in the database, in the order they will be applied:

```shell
% relocant list-unapplied --directory ./migration
001     001-initial-schema      dfde7438
002     002-description         74f8a76e
```

Now, if we like this plan, we can run `relocant apply` to actually apply the scripts to our database.
Each script is run in a separate transaction and recorded as a part of it. Unfortunately, this means
there are restrictions on what you can put in a migration script, including these:

  1. Using `BEGIN`, `ROLLBACK`, and `COMMIT` will likely break _relocant_.
  2. Running `CREATE INDEX CONCURRENTLY` will cause the migration script to fail immediately.

```shell
% relocant apply --directory ./migration
{"_at":...","_severity":"info","action":"apply","migrations":["001","002"]}
{"_at":...","_severity":"info","action":"apply","apply":"001"}
{"_at":...","_severity":"info","action":"apply","record":"001"}
{"_at":...","_severity":"info","action":"apply","apply":"002"}
{"_at":...","_severity":"info","action":"apply","record":"002"}
```

By running `relocant list-applied`, we can check that the applied scripts have been recorded correctly.
Naturally, the recorded migrations will be sorted in the order of application:

```shell
% relocant list-applied
001     001-initial-schema  dfde7438        2024-10-24 15:04:09 +0000       0.01s
002     002-description     74f8a76e        2024-10-24 15:04:09 +0000       0.00s
```

Once you have another migration script to apply, you put it in the `migration` directory and run `relocant apply` again:

```shell
% relocant list-unapplied --directory ./migration
003     003-fix-typo    a7032e4f
% relocant apply
{"_at":"...","_severity":"info","action":"apply","migrations":["003"]}
{"_at":"...","_severity":"info","action":"apply","run":"003"}
{"_at":"...","_severity":"info","action":"apply","record":"003"}
% relocant list-applied --directory ./migration
001     001-initial-schema  dfde7438        2024-10-24 15:04:09 +0000       0.01s
002     002-description     74f8a76e        2024-10-24 15:04:09 +0000       0.00s
003     003-fix-typo        a3582319        2024-10-24 15:24:09 +0000       0.00s
```

Acknowledgements
---

I've been using a (very) similar workflow for a while and it worked great for me, but I wouldn't have
bothered to make it a standalone tool if I haven't stumbled upon [_pgmigrate_][0] by Peter Downs.

  [0]: https://github.com/peterldowns/pgmigrate
