import {useQuery, useMutation, useQueryClient, QueryClient, QueryClientProvider,} from '@tanstack/react-query'

import { NavBar } from "./sections";
import { TodoApp } from "./sections";

const queryClient = new QueryClient()
const initialTasks = [];

function App() {
  return (
    <QueryClientProvider client={queryClient}>
      <>
        <NavBar />
        <TodoApp
          initialTasks={initialTasks}
          initialFilter={"All"}/>
      </>
    </QueryClientProvider>
  )
}

export default App