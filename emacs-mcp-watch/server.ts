import { FastMCP, UserError } from "fastmcp";
import { z } from "zod";
import { existsSync, mkdirSync } from "fs";
import { readFile, watch, writeFile, unlink } from "fs/promises";
import { dirname, basename, join } from "path";
import { randomUUID } from "crypto";

// types

/** EmacsResponse is response to the command sent to Emacs. */
const EmacsResponse = z.object({
  id: z.string(),
  result: z.any().optional(),
  error: z.string().optional(),
});

// instantiate the FastMCP server

const watchDir = process.env.EMACS_BRIDGE_WATCH_DIR || "/tmp/emacs-mcp-bridge";
if (!existsSync(watchDir)) {
  mkdirSync(watchDir, { recursive: true });
}

const server = new FastMCP({
  name: "Emacs",
  version: "0.0.1",
});

server.addTool({
  name: "get-point",
  description: "Returns the point position in the current buffer",
  parameters: z.object({}),
  execute: async () => {
    const result = await callEmacs("get-point");
    return {
      content: [
        {
          type: "text",
          text: result.toString(),
        },
      ],
    };
  },
});

server.addTool({
  name: "get-line-number",
  description: "Returns the line number in the current buffer",
  parameters: z.object({}),
  execute: async () => {
    const result = await callEmacs("get-line-number");
    return {
      content: [
        {
          type: "text",
          text: result.toString(),
        },
      ],
    };
  },
});

server.addTool({
  name: "get-buffer-name",
  description: "Returns the name of the current buffer",
  parameters: z.object({}),
  execute: async () => {
    const result = await callEmacs("get-buffer-name");
    return {
      content: [
        {
          type: "text",
          text: result.toString(),
        },
      ],
    };
  },
});

server.addTool({
  name: "get-project-name",
  description: "Returns the name of the current project (if any)",
  parameters: z.object({}),
  execute: async () => {
    const result = await callEmacs("get-project-name");
    return {
      content: [
        {
          type: "text",
          text: result.toString(),
        },
      ],
    };
  },
});

server.addTool({
  name: "list-project-files",
  description: "Lists all files in the current project",
  parameters: z.object({}),
  execute: async () => {
    const result = await callEmacs("list-project-files");
    return {
      content: [
        {
          type: "text",
          text: Array.isArray(result) ? result.join('\n') : result.toString(),
        },
      ],
    };
  },
});

server.addTool({
  name: "read-file",
  description: "Returns the contents of a file given its filename",
  parameters: z.object({
    filename: z.string().describe("The name of the file to read"),
  }),
  execute: async ({ filename }) => {
    const result = await callEmacs("read-file", { filename });
    return {
      content: [
        {
          type: "text",
          text: result.toString(),
        },
      ],
    };
  },
});

server.addTool({
  name: "fetch-webpage",
  description: "Fetches a web page and returns its text content using Emacs shr renderer",
  parameters: z.object({
    url: z.string().describe("The URL of the web page to fetch"),
  }),
  execute: async ({ url }) => {
    const result = await callEmacs("fetch-webpage", { url });
    return {
      content: [
        {
          type: "text",
          text: result.toString(),
        },
      ],
    };
  },
});

server.start({
  transportType: "stdio",
});

/**
 * Calls Emacs with the given command and parameters.
 *
 * @param command - The command to execute in Emacs.
 * @param params - The parameters to pass to the command.
 * @returns - The result of the command execution.
 * * @throws {@link TimeoutError} - If the response is not received within the timeout period.
 *
 */
async function callEmacs(command: string, params: any = null): Promise<any> {
  const id = randomUUID().replace(/-/g, "");
  const commandFile = join(watchDir, `command-${id}.json`);
  const responseFile = join(watchDir, `response-${id}.json`);

  try {
    // write command the caller wants Emacs to execute to the commands directory
    // on the Emacs side a bridge will watch this directory for files
    // when it sees a new file, it will read the command and execute it
    // the result will be written to a response file with the same ID in the same directory

    await writeFile(
      commandFile,
      JSON.stringify({
        id,
        command,
        params,
      }),
    );

    // wait for a response from Emacs

    const responseJSON = await waitForResponse(responseFile);
    const response = await EmacsResponse.parseAsync(JSON.parse(responseJSON));

    // the response could have an error, or it could not
    // if it has an error we throw; otherwise we return the result

    if (response.error) {
      throw new UserError(`received error from Emacs: ${response.error}`);
    }
    return response.result;
  } finally {
    deleteFileIfExists(commandFile);
    deleteFileIfExists(responseFile);
  }
}

/**
 * Wait for a response from Emacs by watching the command directory.
 *
 * @param filePath - The path to the response to wait for.
 * @param timeoutMs - How long to wait for the response in milliseconds.
 * @returns - The content of the response file.
 * @throws {TimeoutError} - If the response is not received within the timeout period.
 *
 */
async function waitForResponse(
  filePath: string,
  timeoutMs: number = 10000,
): Promise<string> {
  const timeoutError = new UserError("timeout waiting for response from Emacs");

  const ac = new AbortController();
  const { signal } = ac;
  setTimeout(() => ac.abort(), timeoutMs);

  try {
    const watcher = watch(dirname(filePath), { signal });
    for await (const event of watcher) {
      if (
        event.filename === basename(filePath) &&
        event.eventType === "rename"
      ) {
        // handle the file not being fully written yet
        for (let attempt = 0; attempt < 3; attempt++) {
          try {
            return await readFile(filePath, "utf8");
          } catch (err: any) {
            if (err.code === "ENOENT" && attempt < 2) {
              await new Promise((resolve) =>
                setTimeout(resolve, 10 * Math.pow(2, attempt)),
              );
              continue;
            }
            throw err;
          }
        }
      }
    }
  } catch (err: any) {
    if (err.name === "AbortError") {
      throw timeoutError;
    }
    throw err;
  }

  throw timeoutError;
}

/**
 * Delete a file if it exists.
 *
 * @param filePath - The path to the file to delete.
 */
async function deleteFileIfExists(filePath: string): Promise<void> {
  try {
    await unlink(filePath);
  } catch (err: any) {
    if (err.code !== "ENOENT") {
      console.error(`Failed to delete file ${filePath}:`, err);
    }
  }
}
